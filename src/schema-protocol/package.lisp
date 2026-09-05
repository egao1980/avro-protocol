(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((pkg (find-package '#:schema-protocol))
         (sym (intern "AVRO-SCHEMA" pkg)))
    (export sym pkg)))

(defpackage #:schema-protocol-avro
  (:use #:cl)
  (:nicknames #:stack-schema-avro)
  (:import-from #:closer-mop
                #:slot-definition-type)
  (:import-from #:schema-protocol
                #:schema-of
                #:schema-slots
                #:find-schema
                #:schema-tag
                #:schema-variants
                #:enum-of
                #:enum-members
                #:finalize-schema
                #:type-kind
                #:type-args
                #:sequence-element-type
                #:slot-is-required-p
                #:slot-optional-p
                #:slot-wire-p
                #:slot-dump-p
                #:slot-wire-key
                #:slot-minimum
                #:slot-maximum
                #:avro-schema
                #:schema-error)
  (:export #:avro-schema-error
           #:avro-schema-error-message
           #:emit
           #:avro-schema))

(in-package #:schema-protocol-avro)

(unless (and (fboundp 'avro-schema)
             (typep (symbol-function 'avro-schema) 'generic-function))
  (defgeneric avro-schema (schema &key)
    (:documentation "Emit an Avro schema tree.")))
