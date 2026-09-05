(in-package #:schema-protocol-avro)

(defmethod avro-schema ((schema symbol) &key)
  (emit schema))

(defmethod avro-schema ((schema standard-object) &key)
  (emit schema))
