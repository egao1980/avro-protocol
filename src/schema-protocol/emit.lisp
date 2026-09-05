(in-package #:schema-protocol-avro)

(define-condition avro-schema-error (schema-error)
  ((message :initarg :message :reader avro-schema-error-message :initform nil))
  (:report (lambda (c s)
             (format s "Avro schema error~@[: ~A~]" (avro-schema-error-message c)))))

(defun %or-null-inner (spec)
  (if (eq (type-kind spec) :or)
      (let ((args (type-args spec)))
        (cond
          ((and (= 2 (length args))
                (or (eq (type-kind (first args)) :null)
                    (eq (first args) :null)))
           (values (second args) t))
          ((and (= 2 (length args))
                (or (eq (type-kind (second args)) :null)
                    (eq (second args) :null)))
           (values (first args) t))
          (t (values spec nil))))
      (values spec nil)))

(defun %ht (&rest plist)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          when v
            do (setf (gethash k ht) v))
    ht))

(defun type-avro (spec slot)
  (multiple-value-bind (inner nullable) (%or-null-inner spec)
    (let* ((kind (type-kind inner))
           (avro (case kind
                    (:null "null")
                    (:string "string")
                    (:boolean "boolean")
                    ((:float :real :number) "double")
                    (:integer
                     (let ((lo (or (and slot (slot-minimum slot))
                                   (first (type-args inner))))
                           (hi (or (and slot (slot-maximum slot))
                                   (second (type-args inner)))))
                       (if (and (integerp lo) (integerp hi)
                                (>= lo (- (expt 2 31)))
                                (<= hi (1- (expt 2 31))))
                           "int"
                           "long")))
                    ((:keyword :symbol) "string")
                    (:member
                     (%ht "type" "enum"
                          "name" (format nil "Member~A" (sxhash spec))
                          "symbols" (map 'vector
                                         (lambda (s)
                                           (if (symbolp s)
                                               (string-downcase (symbol-name s))
                                               (princ-to-string s)))
                                         (type-args inner))))
                    (:enum
                     (let ((e (enum-of inner)))
                       (%ht "type" "enum"
                            "name" (string-downcase (symbol-name (if (symbolp inner) inner 'enum)))
                            "symbols" (map 'vector
                                           (lambda (s)
                                             (string-downcase (symbol-name s)))
                                           (if e (enum-members e) (type-args inner))))))
                    (:eql "string")
                    ((:vector :list :sequence)
                     (%ht "type" "array"
                          "items" (type-avro (or (sequence-element-type inner slot) 'string) nil)))
                    (:nested
                     (emit inner))
                    (:hash-table
                     (%ht "type" "map" "values" "string"))
                    (t "string"))))
      (if nullable
          (coerce (list "null" avro) 'vector)
          avro))))

(defun emit (schema)
  "defschema / schema-class / instance → Avro schema tree (hash-table / string / union vector)."
  (let ((class (schema-of schema)))
    (finalize-schema class)
    (when (and (schema-tag class) (schema-variants class))
      (return-from emit
        (coerce (mapcar #'emit (schema-variants class)) 'vector)))
    (let ((fields '()))
      (dolist (slot (schema-slots class))
        (when (and (slot-wire-p slot) (slot-dump-p slot))
          (let* ((spec (slot-definition-type slot))
                 (field (%ht "name" (slot-wire-key slot class)
                             "type" (type-avro spec slot))))
            (when (eq (type-kind spec) :eql)
              (setf (gethash "default" field)
                    (let ((v (second spec)))
                      (if (symbolp v)
                          (string-downcase (symbol-name v))
                          v))))
            (push field fields))))
      (%ht "type" "record"
           "name" (string-downcase (symbol-name (class-name class)))
           "fields" (coerce (nreverse fields) 'vector)))))
