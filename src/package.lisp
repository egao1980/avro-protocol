(defpackage #:avro-protocol
  (:use #:cl)
  (:nicknames #:stack-avro)
  (:export #:avro-error
           #:avro-encode-error
           #:avro-decode-error
           #:avro-schema-error
           #:avro-error-message

           #:avro-schema
           #:avro-schema-p
           #:avro-schema-kind
           #:avro-schema-name
           #:avro-schema-fullname
           #:parse-schema
           #:schema-json

           #:*avro-schema*
           #:*avro-reader-schema*
           #:*avro-backend*
           #:avro-backend
           #:backend-encode
           #:backend-decode
           #:make-avro-backend
           #:use-avro-backend

           #:encode
           #:decode
           #:encode-to-octets
           #:decode-octets

           #:avro-serdes-backend
           #:make-avro-serdes-backend
           #:use-avro-serdes-backend
           #:install-http-avro-hooks))

(in-package #:avro-protocol)
