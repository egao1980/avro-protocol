(in-package #:schema-protocol-avro/tests)

(deftest emit-record
  (schema-protocol:defschema %avro-user ()
    (id integer)
    (name string)
    (email (or :null string) :optional t))
  (let ((schema (schema-protocol:avro-schema '%avro-user)))
    (ok (hash-table-p schema))
    (ok (string= "record" (gethash "type" schema)))
    (ok (string= "%avro-user" (gethash "name" schema)))
    (let* ((octets (avro-protocol:encode
                    (let ((ht (make-hash-table :test #'equal)))
                      (setf (gethash "id" ht) 1
                            (gethash "name" ht) "ada"
                            (gethash "email" ht) :null)
                      ht)
                    :schema schema))
           (back (avro-protocol:decode octets :schema schema)))
      (ok (= 1 (gethash "id" back)))
      (ok (string= "ada" (gethash "name" back)))
      (ok (eq :null (gethash "email" back))))))
