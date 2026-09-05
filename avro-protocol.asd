(defsystem "avro-protocol"
  :version "0.1.0"
  :description "CLOS Avro binary encode/decode for cl-stack (writer schema required); implements serdes-protocol :avro"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
  :properties (:cl-repo (:ci (:with ("schema-protocol-avro"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "json")
               (:file "schema")
               (:file "codec")
               (:file "protocol")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "avro-protocol/tests"))))

(defsystem "avro-protocol/tests"
  :depends-on ("avro-protocol" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "codec-test")
               (:file "serdes-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
