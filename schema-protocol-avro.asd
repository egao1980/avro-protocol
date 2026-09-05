(defsystem "schema-protocol-avro"
  :version "0.1.2"
  :description "Avro schema emit/parse for schema-protocol (defschema ↔ Avro JSON schema)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("schema-protocol" "avro-protocol" "closer-mop")
:serial t
  :pathname "src/schema-protocol"
  :components ((:file "package")
               (:file "emit")
               (:file "compile")
               (:file "protocol"))
  :in-order-to ((test-op (test-op "schema-protocol-avro/tests"))))

(defsystem "schema-protocol-avro/tests"
  :depends-on ("schema-protocol-avro" "avro-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "schema-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
