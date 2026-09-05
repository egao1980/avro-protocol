(in-package #:avro-protocol)

(defun %u8 (n)
  (make-array n :element-type '(unsigned-byte 8) :fill-pointer 0 :adjustable t))

(defun %push (buf byte)
  (vector-push-extend byte buf))

(defun %write-varint (buf n)
  (let ((v (logand n #xffffffffffffffff)))
    (loop
      (let ((b (logand v #x7f)))
        (setf v (ash v -7))
        (if (zerop v)
            (return (%push buf b))
            (%push buf (logior b #x80)))))))

(defun %write-long (buf n)
  (%write-varint buf (logxor (ash n 1) (ash n -63))))

(defun %octet-vector-p (value)
  (and (vectorp value)
       (not (stringp value))
       (let ((et (array-element-type value)))
         (or (equal et '(unsigned-byte 8))
             (and (not (eq et t)) (subtypep et '(unsigned-byte 8)))))))

(defun %key-string (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))
    (character (string key))))

(defun %table-ref (table key)
  (cond
    ((hash-table-p table)
     (or (gethash key table)
         (gethash (%key-string key) table)
         (let ((found nil) (hit nil))
           (maphash (lambda (k v)
                      (when (string= (%key-string k) (%key-string key))
                        (setf found v hit t)))
                    table)
           (if hit found (values nil nil)))))
    ((and (listp table) (consp (first table)))
     (cdr (or (assoc key table :test #'equal)
              (assoc (%key-string key) table :test #'string-equal))))
    ((listp table)
     (getf table (intern (string-upcase (%key-string key)) :keyword)))
    (t nil)))

#+sbcl
(defun %write-float32 (buf value)
  (let ((bits (sb-kernel:single-float-bits (float value 1.0f0))))
    (loop for shift from 0 to 24 by 8
          do (%push buf (ldb (byte 8 shift) (logand bits #xffffffff))))))

#+sbcl
(defun %write-float64 (buf value)
  (let ((hi (sb-kernel:double-float-high-bits (float value 1.0d0)))
        (lo (sb-kernel:double-float-low-bits (float value 1.0d0))))
    (loop for shift from 0 to 24 by 8
          do (%push buf (ldb (byte 8 shift) lo)))
    (loop for shift from 0 to 24 by 8
          do (%push buf (ldb (byte 8 shift) (logand hi #xffffffff))))))

#-sbcl
(defun %write-float32 (buf value)
  (declare (ignore buf value))
  (error 'avro-encode-error :message "Avro float encode requires SBCL in 0.1.0"))

#-sbcl
(defun %write-float64 (buf value)
  (declare (ignore buf value))
  (error 'avro-encode-error :message "Avro double encode requires SBCL in 0.1.0"))

(defun %union-branch (schema value)
  (loop for branch in (avro-schema-branches schema)
        for i from 0
        when (%matches-p branch value)
          do (return (values i branch))
        finally (error 'avro-encode-error
                       :message (format nil "value ~S matches no union branch"
                                        (type-of value)))))

(defun %matches-p (schema value)
  (ecase (avro-schema-kind schema)
    (:null (eq value :null))
    (:boolean (or (eq value t) (null value)))
    ((:int :long) (integerp value))
    ((:float :double) (or (floatp value) (integerp value)))
    (:bytes (%octet-vector-p value))
    (:string (stringp value))
    (:enum (let ((s (if (symbolp value) (string-downcase (symbol-name value)) value)))
             (and (stringp s) (find s (avro-schema-symbols schema) :test #'string=))))
    (:fixed (and (%octet-vector-p value)
                 (= (length value) (avro-schema-size schema))))
    (:array (and (or (vectorp value) (listp value)) (not (stringp value))))
    (:map (or (hash-table-p value) (and (listp value) (consp (first value)))))
    (:record (or (hash-table-p value) (listp value) (typep value 'standard-object)))
    (:union (nth-value 1 (%union-branch schema value)))))

(defun encode-with-schema (schema value buf)
  (ecase (avro-schema-kind schema)
    (:null
     (unless (eq value :null)
       (error 'avro-encode-error :message "expected Avro null"))
     buf)
    (:boolean
     (%push buf (if value 1 0)))
    (:int
     (%write-long buf value))
    (:long
     (%write-long buf value))
    (:float
     (%write-float32 buf value))
    (:double
     (%write-float64 buf value))
    (:bytes
     (let ((octets (if (stringp value)
                       (babel:string-to-octets value :encoding :utf-8)
                       value)))
       (%write-long buf (length octets))
       (loop for b across octets do (%push buf b))))
    (:string
     (let ((octets (babel:string-to-octets (if (symbolp value)
                                               (string-downcase (symbol-name value))
                                               value)
                                           :encoding :utf-8)))
       (%write-long buf (length octets))
       (loop for b across octets do (%push buf b))))
    (:enum
     (let* ((s (if (symbolp value) (string-downcase (symbol-name value)) value))
            (idx (position s (avro-schema-symbols schema) :test #'string=)))
       (unless idx
         (error 'avro-encode-error :message (format nil "unknown enum symbol ~S" s)))
       (%write-long buf idx)))
    (:fixed
     (unless (and (%octet-vector-p value) (= (length value) (avro-schema-size schema)))
       (error 'avro-encode-error :message "fixed length mismatch"))
     (loop for b across value do (%push buf b)))
    (:array
     (let ((seq (if (listp value) (coerce value 'vector) value)))
       (when (plusp (length seq))
         (%write-long buf (length seq))
         (loop for item across seq
               do (encode-with-schema (avro-schema-items schema) item buf)))
       (%write-long buf 0)))
    (:map
     (let ((pairs '()))
       (cond
         ((hash-table-p value)
          (maphash (lambda (k v) (push (cons (%key-string k) v) pairs)) value))
         (t
          (dolist (p value)
            (push (cons (%key-string (car p)) (cdr p)) pairs))))
       (when pairs
         (%write-long buf (length pairs))
         (dolist (p (nreverse pairs))
           (encode-with-schema (%make-avro-schema :kind :string) (car p) buf)
           (encode-with-schema (avro-schema-values schema) (cdr p) buf)))
       (%write-long buf 0)))
    (:record
     (dolist (field (avro-schema-fields schema))
       (multiple-value-bind (raw found)
           (let ((v (%table-ref value (avro-field-name field))))
             (values v (or v (avro-field-default-bound-p field) t)))
         (declare (ignore found))
         (let ((v (if (and (null raw) (avro-field-default-bound-p field)
                           (not (%table-ref value (avro-field-name field))))
                      (avro-field-default field)
                      raw)))
           (encode-with-schema (avro-field-type field) v buf)))))
    (:union
     (multiple-value-bind (idx branch) (%union-branch schema value)
       (%write-long buf idx)
       (encode-with-schema branch value buf)))))

(defstruct (%reader (:conc-name %r-) (:constructor %make-reader (octets &optional (index 0))))
  octets index)

(defun %need (reader n)
  (unless (<= (+ (%r-index reader) n) (length (%r-octets reader)))
    (error 'avro-decode-error :message "truncated Avro")))

(defun %read-byte (reader)
  (%need reader 1)
  (prog1 (aref (%r-octets reader) (%r-index reader))
    (incf (%r-index reader))))

(defun %read-varint (reader)
  (let ((shift 0)
        (n 0))
    (loop
      (let ((b (%read-byte reader)))
        (setf n (logior n (ash (logand b #x7f) shift)))
        (when (zerop (logand b #x80))
          (return n))
        (incf shift 7)
        (when (> shift 63)
          (error 'avro-decode-error :message "Avro varint too long"))))))

(defun %read-long (reader)
  (let ((n (%read-varint reader)))
    (logxor (ash n -1) (- (logand n 1)))))

(defun %read-bytes (reader n)
  (%need reader n)
  (let ((out (make-array n :element-type '(unsigned-byte 8))))
    (replace out (%r-octets reader) :start2 (%r-index reader))
    (incf (%r-index reader) n)
    out))

#+sbcl
(defun %read-float32 (reader)
  (let ((bits 0))
    (loop for shift from 0 to 24 by 8
          do (setf bits (logior bits (ash (%read-byte reader) shift))))
    (sb-kernel:make-single-float (if (> bits #x7fffffff)
                                     (- bits (ash 1 32))
                                     bits))))

#+sbcl
(defun %read-float64 (reader)
  (let ((lo 0)
        (hi 0))
    (loop for shift from 0 to 24 by 8
          do (setf lo (logior lo (ash (%read-byte reader) shift))))
    (loop for shift from 0 to 24 by 8
          do (setf hi (logior hi (ash (%read-byte reader) shift))))
    (sb-kernel:make-double-float (if (> hi #x7fffffff)
                                     (- hi (ash 1 32))
                                     hi)
                                 lo)))

#-sbcl
(defun %read-float32 (reader)
  (declare (ignore reader))
  (error 'avro-decode-error :message "Avro float decode requires SBCL in 0.1.0"))

#-sbcl
(defun %read-float64 (reader)
  (declare (ignore reader))
  (error 'avro-decode-error :message "Avro double decode requires SBCL in 0.1.0"))

(defun %promote (value writer reader)
  (let ((wk (avro-schema-kind writer))
        (rk (avro-schema-kind reader)))
    (cond
      ((eq wk rk) value)
      ((and (eq wk :int) (member rk '(:long :float :double))) value)
      ((and (eq wk :long) (member rk '(:float :double))) value)
      ((and (eq wk :float) (eq rk :double)) (float value 1.0d0))
      ((and (eq wk :string) (eq rk :bytes))
       (babel:string-to-octets value :encoding :utf-8))
      ((and (eq wk :bytes) (eq rk :string))
       (babel:octets-to-string value :encoding :utf-8))
      (t value))))

(defun %resolve (writer reader)
  (or reader writer))

(defun decode-with-schema (writer reader-schema octet-reader)
  (let ((reader (%resolve writer reader-schema)))
    (labels ((walk (w r rd)
               (let ((rk (avro-schema-kind r))
                     (wk (avro-schema-kind w)))
                 (when (and (eq wk :union) (not (eq rk :union)))
                   (let ((idx (%read-long rd))
                         (branches (avro-schema-branches w)))
                     (when (or (minusp idx) (>= idx (length branches)))
                       (error 'avro-decode-error :message "union index out of range"))
                     (return-from walk (walk (nth idx branches) r rd))))
                 (when (and (eq rk :union) (not (eq wk :union)))
                   (return-from walk (walk w w rd)))
                 (ecase wk
                   (:null :null)
                   (:boolean (plusp (%read-byte rd)))
                   (:int (%promote (%read-long rd) w r))
                   (:long (%promote (%read-long rd) w r))
                   (:float (%promote (%read-float32 rd) w r))
                   (:double (%promote (%read-float64 rd) w r))
                   (:bytes
                    (let ((v (%read-bytes rd (%read-long rd))))
                      (%promote v w r)))
                   (:string
                    (let ((v (babel:octets-to-string (%read-bytes rd (%read-long rd))
                                                     :encoding :utf-8)))
                      (%promote v w r)))
                   (:enum
                    (let* ((idx (%read-long rd))
                           (sym (nth idx (avro-schema-symbols w))))
                      (or (and (eq rk :enum)
                               (or (find sym (avro-schema-symbols r) :test #'string=)
                                   (avro-schema-default r)))
                          sym)))
                   (:fixed (%read-bytes rd (avro-schema-size w)))
                   (:array
                    (let ((acc (make-array 8 :adjustable t :fill-pointer 0))
                          (item-r (if (eq rk :array) (avro-schema-items r) (avro-schema-items w))))
                      (loop
                        (let ((count (%read-long rd)))
                          (when (zerop count)
                            (return (coerce acc 'vector)))
                          (when (minusp count)
                            (%read-long rd)
                            (setf count (- count)))
                          (dotimes (i count)
                            (vector-push-extend (walk (avro-schema-items w) item-r rd) acc))))))
                   (:map
                    (let ((ht (make-hash-table :test #'equal))
                          (val-r (if (eq rk :map) (avro-schema-values r) (avro-schema-values w))))
                      (loop
                        (let ((count (%read-long rd)))
                          (when (zerop count)
                            (return ht))
                          (when (minusp count)
                            (%read-long rd)
                            (setf count (- count)))
                          (dotimes (i count)
                            (let ((k (walk (%make-avro-schema :kind :string)
                                           (%make-avro-schema :kind :string)
                                           rd)))
                              (setf (gethash k ht) (walk (avro-schema-values w) val-r rd))))))))
                   (:record
                    (let ((out (make-hash-table :test #'equal))
                          (rfields (if (eq rk :record) (avro-schema-fields r) (avro-schema-fields w))))
                      (dolist (wf (avro-schema-fields w))
                        (let* ((rf (find (avro-field-name wf) rfields
                                         :key #'avro-field-name :test #'string=))
                               (v (walk (avro-field-type wf)
                                        (if rf (avro-field-type rf) (avro-field-type wf))
                                        rd)))
                          (when (or rf (eq r w))
                            (setf (gethash (avro-field-name wf) out) v))))
                      (dolist (rf rfields)
                        (unless (nth-value 1 (gethash (avro-field-name rf) out))
                          (when (avro-field-default-bound-p rf)
                            (setf (gethash (avro-field-name rf) out)
                                  (avro-field-default rf)))))
                      out))
                   (:union
                    (let ((idx (%read-long rd))
                          (branches (avro-schema-branches w)))
                      (when (or (minusp idx) (>= idx (length branches)))
                        (error 'avro-decode-error :message "union index out of range"))
                      (walk (nth idx branches)
                            (if (eq rk :union)
                                (or (find (avro-schema-kind (nth idx branches))
                                          (avro-schema-branches r)
                                          :key #'avro-schema-kind)
                                    (nth idx branches))
                                r)
                            rd)))))))
      (walk writer reader octet-reader))))

(defun encode-avro (schema value)
  (let ((buf (%u8 64)))
    (encode-with-schema (parse-schema schema) value buf)
    (coerce buf '(simple-array (unsigned-byte 8) (*)))))

(defun decode-avro (schema octets &optional reader-schema)
  (let ((rd (%make-reader octets))
        (writer (parse-schema schema))
        (rsch (and reader-schema (parse-schema reader-schema))))
    (prog1 (decode-with-schema writer rsch rd)
      (unless (= (%r-index rd) (length octets))
        (error 'avro-decode-error :message "trailing Avro bytes")))))
