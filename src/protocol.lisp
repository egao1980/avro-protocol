(in-package #:avro-protocol)

(defvar *avro-schema* nil
  "Writer schema for encode / serdes :avro.")

(defvar *avro-reader-schema* nil
  "Optional reader schema for resolution on decode.")

(defvar *avro-backend* nil)

(defclass avro-backend () ())

(defgeneric backend-encode (backend value &key stream schema))
(defgeneric backend-decode (backend source &key schema reader-schema))

(defun %ensure-schema (schema)
  (parse-schema
   (or schema *avro-schema*
       (error 'avro-schema-error
              :message "*avro-schema* is nil — bind it or pass :schema"))))

(defun %source-octets (source)
  (etypecase source
    ((vector (unsigned-byte 8)) source)
    (vector
     (if (and (not (stringp source))
              (every (lambda (b) (typep b '(unsigned-byte 8))) source))
         (coerce source '(vector (unsigned-byte 8)))
         (error 'avro-decode-error :message "Avro decode needs octets")))
    (stream
     (let ((buf (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
           (tmp (make-array 4096 :element-type '(unsigned-byte 8))))
       (loop for n = (read-sequence tmp source)
             do (loop for i from 0 below n do (vector-push-extend (aref tmp i) buf))
             until (< n 4096))
       (coerce buf '(vector (unsigned-byte 8)))))))

(defclass native-avro-backend (avro-backend) ())

(defun make-avro-backend ()
  (make-instance 'native-avro-backend))

(defmethod backend-encode ((backend native-avro-backend) value &key stream schema)
  (declare (ignore backend))
  (let ((octets (encode-avro (%ensure-schema schema) value)))
    (if stream
        (progn (write-sequence octets stream) (values))
        octets)))

(defmethod backend-decode ((backend native-avro-backend) source
                           &key schema reader-schema)
  (declare (ignore backend))
  (decode-avro (%ensure-schema schema)
               (%source-octets source)
               (or reader-schema *avro-reader-schema*)))

(defun use-avro-backend ()
  (setf *avro-backend* (make-avro-backend)))

(defun encode (value &key stream schema)
  (unless *avro-backend*
    (error 'avro-encode-error :message "*avro-backend* is unbound — load avro-protocol"))
  (backend-encode *avro-backend* value :stream stream :schema schema))

(defun decode (source &key schema reader-schema)
  (unless *avro-backend*
    (error 'avro-decode-error :message "*avro-backend* is unbound — load avro-protocol"))
  (backend-decode *avro-backend* source :schema schema :reader-schema reader-schema))

(defun encode-to-octets (value &key schema)
  (encode value :schema schema))

(defun decode-octets (octets &key schema reader-schema)
  (decode octets :schema schema :reader-schema reader-schema))

(defun install-http-avro-hooks ()
  "If http-protocol is loaded, register :avro data (de)serializers."
  (let ((pkg (find-package :http-protocol)))
    (unless pkg
      (return-from install-http-avro-hooks nil))
    (flet ((push-codec (table-sym type fn)
             (let ((s (find-symbol table-sym pkg)))
               (when (and s (boundp s))
                 (setf (symbol-value s)
                       (acons type fn (remove type (symbol-value s) :key #'car)))))))
      (push-codec "*DATA-SERIALIZERS*" :avro #'encode)
      (push-codec "*DATA-DESERIALIZERS*" :avro #'decode))
    t))

(eval-when (:load-toplevel :execute)
  (use-avro-backend)
  (install-http-avro-hooks))
