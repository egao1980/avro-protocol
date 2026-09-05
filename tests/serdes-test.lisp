(in-package #:avro-protocol/tests)

(deftest serdes-avro
  (let* ((schema "\"string\"")
         (*avro-schema* schema)
         (octets (serdes-protocol:encode "z" :format :avro)))
    (ok (string= "z" (serdes-protocol:decode octets :format :avro)))
    (ok (serdes-protocol:format-binary-p :avro))
    (ok (string= "application/avro" (serdes-protocol:format-media-type :avro)))
    (ok (eq :avro (serdes-protocol:find-format-for-media-type "avro/binary")))))
