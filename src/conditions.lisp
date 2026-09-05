(in-package #:avro-protocol)

(define-condition avro-error (error)
  ((message :initarg :message :reader avro-error-message :initform nil))
  (:report (lambda (c s)
             (format s "Avro error~@[: ~a~]" (avro-error-message c)))))

(define-condition avro-encode-error (avro-error) ())

(define-condition avro-decode-error (avro-error) ())

(define-condition avro-schema-error (avro-error) ())
