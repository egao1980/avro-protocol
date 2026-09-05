(in-package #:avro-protocol/tests)

(defun %ht (&rest plist)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash k ht) v))
    ht))

(deftest primitive-roundtrip
  (dolist (pair '(("\"null\"" :null)
                  ("\"boolean\"" t)
                  ("\"boolean\"" nil)
                  ("\"int\"" 42)
                  ("\"int\"" -7)
                  ("\"long\"" 1000000)
                  ("\"string\"" "hi")))
    (destructuring-bind (schema value) pair
      (ok (equalp value (decode (encode value :schema schema) :schema schema))
          (format nil "~A ~S" schema value)))))

(deftest record-roundtrip
  (let* ((schema "{\"type\":\"record\",\"name\":\"User\",\"fields\":[{\"name\":\"id\",\"type\":\"long\"},{\"name\":\"name\",\"type\":\"string\"}]}")
         (value (%ht "id" 1 "name" "ada"))
         (back (decode (encode value :schema schema) :schema schema)))
    (ok (= 1 (gethash "id" back)))
    (ok (string= "ada" (gethash "name" back)))))

(deftest union-null-string
  (let* ((schema "[\"null\",\"string\"]")
         (a (decode (encode :null :schema schema) :schema schema))
         (b (decode (encode "x" :schema schema) :schema schema)))
    (ok (eq :null a))
    (ok (string= "x" b))))

(deftest array-and-map
  (let* ((as "{\"type\":\"array\",\"items\":\"int\"}")
         (ms "{\"type\":\"map\",\"values\":\"string\"}")
         (arr (decode (encode #(1 2 3) :schema as) :schema as))
         (mp (decode (encode (%ht "a" "b") :schema ms) :schema ms)))
    (ok (equalp #(1 2 3) arr))
    (ok (string= "b" (gethash "a" mp)))))

(deftest reader-schema-adds-default
  (let* ((writer "{\"type\":\"record\",\"name\":\"U\",\"fields\":[{\"name\":\"id\",\"type\":\"int\"}]}")
         (reader "{\"type\":\"record\",\"name\":\"U\",\"fields\":[{\"name\":\"id\",\"type\":\"int\"},{\"name\":\"nick\",\"type\":\"string\",\"default\":\"x\"}]}")
         (back (decode (encode (%ht "id" 3) :schema writer)
                       :schema writer :reader-schema reader)))
    (ok (= 3 (gethash "id" back)))
    (ok (string= "x" (gethash "nick" back)))))
