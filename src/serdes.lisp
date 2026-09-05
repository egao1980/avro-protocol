(in-package #:avro-protocol)

(defclass avro-serdes-backend (serdes-protocol:serdes-backend) ())

(defun make-avro-serdes-backend ()
  (make-instance 'avro-serdes-backend))

(defmethod serdes-protocol:backend-media-type ((backend avro-serdes-backend))
  "application/avro")

(defmethod serdes-protocol:backend-binary-p ((backend avro-serdes-backend))
  t)

(defun %plist-source-p (source)
  (and (consp source) (keywordp (car source))))

(defun %schema-for-decode (source)
  (or (and (%plist-source-p source) (getf source :schema))
      *avro-schema*
      (error 'avro-decode-error
             :message "*avro-schema* is nil — bind it or pass (:schema SCH :octets OCTETS)")))

(defun %payload-for-decode (source)
  (if (%plist-source-p source)
      (or (getf source :octets) (getf source :data))
      source))

(defmethod serdes-protocol:backend-encode ((backend avro-serdes-backend) value &key stream)
  (declare (ignore backend))
  (encode value :stream stream))

(defmethod serdes-protocol:backend-decode ((backend avro-serdes-backend) source &key)
  (declare (ignore backend))
  (decode (%payload-for-decode source) :schema (%schema-for-decode source)))

(defclass avro-binary-input-stream (serdes-protocol:serdes-binary-input-stream)
  ((done :initform nil :accessor avro-stream-done-p)))

(defclass avro-binary-output-stream (serdes-protocol:serdes-binary-output-stream) ())

(defmethod serdes-protocol:backend-make-input-stream ((backend avro-serdes-backend)
                                                      underlying
                                                      &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'avro-error :message "avro streams are binary"))
  (make-instance 'avro-binary-input-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:backend-make-output-stream ((backend avro-serdes-backend)
                                                       underlying
                                                       &key (element-type '(unsigned-byte 8)))
  (unless (equal element-type '(unsigned-byte 8))
    (error 'avro-error :message "avro streams are binary"))
  (make-instance 'avro-binary-output-stream :underlying underlying :backend backend))

(defmethod serdes-protocol:stream-decode-value ((stream avro-binary-input-stream) &key)
  (if (avro-stream-done-p stream)
      :eof
      (let* ((in (serdes-protocol:underlying-stream stream))
             (buf (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
             (tmp (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop for n = (read-sequence tmp in)
              do (loop for i from 0 below n do (vector-push-extend (aref tmp i) buf))
              until (< n 4096))
        (when (zerop (length buf))
          (return-from serdes-protocol:stream-decode-value :eof))
        (setf (avro-stream-done-p stream) t)
        (decode (coerce buf '(vector (unsigned-byte 8)))))))

(defmethod serdes-protocol:stream-encode-value ((stream avro-binary-output-stream) value &key)
  (write-sequence (encode value) (serdes-protocol:underlying-stream stream))
  value)

(defun use-avro-serdes-backend ()
  (let ((backend (make-avro-serdes-backend)))
    (serdes-protocol:register-format :avro backend
                                     :media-type "application/avro"
                                     :binary t)
    backend))

(eval-when (:load-toplevel :execute)
  (use-avro-serdes-backend))
