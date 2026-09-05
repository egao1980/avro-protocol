(in-package #:schema-protocol-avro)

(defclass avro-schema-backend (schema-format-backend) ()
  (:documentation "schema-protocol format backend for :avro."))

(defmethod backend-emit-schema ((backend avro-schema-backend) schema &key &allow-other-keys)
  (declare (ignore backend))
  (emit schema))

(defmethod backend-parse-schema ((backend avro-schema-backend) source
                                 &key name package &allow-other-keys)
  (declare (ignore backend))
  (apply #'compile-schema source
         (append (when name (list :name name))
                 (when package (list :package package)))))

(eval-when (:load-toplevel :execute)
  (register-schema-format :avro (make-instance 'avro-schema-backend)))

(defmethod avro-schema ((schema symbol) &key)
  (emit schema))

(defmethod avro-schema ((schema standard-object) &key)
  (emit schema))
