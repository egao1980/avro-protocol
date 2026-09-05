(in-package #:avro-protocol)

(defstruct (avro-schema (:constructor %make-avro-schema)
                        (:predicate avro-schema-p))
  kind name namespace fullname
  fields symbols items values size branches aliases default)

(defstruct (avro-field (:constructor %make-avro-field))
  name type default default-bound-p aliases)

(defun %fullname (name namespace)
  (cond
    ((and name (find #\. name :test #'char=)) name)
    ((and name namespace (plusp (length namespace)))
     (format nil "~A.~A" namespace name))
    (t name)))

(defun %table-get (table key)
  (and (hash-table-p table) (gethash key table)))

(defun %as-list (value)
  (cond
    ((null value) nil)
    ((vectorp value) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun %primitive-kind (name)
  (let ((s (string-downcase name)))
    (cond
      ((string= s "null") :null)
      ((string= s "boolean") :boolean)
      ((string= s "int") :int)
      ((string= s "long") :long)
      ((string= s "float") :float)
      ((string= s "double") :double)
      ((string= s "bytes") :bytes)
      ((string= s "string") :string)
      (t nil))))

(defun %compile-schema (source names namespace)
  (cond
    ((avro-schema-p source) source)
    ((stringp source)
     (let ((prim (%primitive-kind source)))
       (if prim
           (%make-avro-schema :kind prim :name source :fullname source)
           (or (gethash source names)
               (or (gethash (%fullname source namespace) names)
                   (error 'avro-schema-error
                          :message (format nil "unknown Avro type ~S" source)))))))
    ((keywordp source)
     (%compile-schema (string-downcase (symbol-name source)) names namespace))
    ((symbolp source)
     (%compile-schema (string-downcase (symbol-name source)) names namespace))
    ((or (and (vectorp source) (not (stringp source)))
         (and (listp source) (not (hash-table-p source))
              (not (avro-schema-p source))))
     (when (and (listp source) (keywordp (first source)))
       (return-from %compile-schema
         (%compile-schema (coerce source 'vector) names namespace)))
     (%make-avro-schema
      :kind :union
      :branches (mapcar (lambda (b) (%compile-schema b names namespace))
                        (%as-list source))))
    ((hash-table-p source)
     (let* ((type (%table-get source "type"))
            (type-name (cond
                         ((stringp type) (string-downcase type))
                         ((symbolp type) (string-downcase (symbol-name type)))
                         (t type)))
            (ns (or (%table-get source "namespace") namespace)))
       (cond
         ((and (not (stringp type))
               (or (vectorp type) (listp type)))
          (%compile-schema type names ns))
         ((and (stringp type-name) (%primitive-kind type-name)
               (not (gethash "fields" source))
               (not (gethash "symbols" source))
               (not (gethash "items" source))
               (not (gethash "values" source))
               (not (gethash "size" source)))
          (%make-avro-schema :kind (%primitive-kind type-name)
                             :name type-name
                             :fullname type-name))
         ((or (equal type-name "record") (gethash "fields" source))
          (let* ((name (%table-get source "name"))
                 (full (%fullname name ns))
                 (schema (%make-avro-schema :kind :record
                                            :name name
                                            :namespace ns
                                            :fullname full
                                            :aliases (%as-list (%table-get source "aliases")))))
            (when full
              (setf (gethash full names) schema)
              (when name (setf (gethash name names) schema)))
            (setf (avro-schema-fields schema)
                  (mapcar (lambda (f)
                            (unless (hash-table-p f)
                              (error 'avro-schema-error
                                     :message "record field must be an object"))
                            (let ((ft (%compile-schema (%table-get f "type") names ns)))
                              (%make-avro-field
                               :name (%table-get f "name")
                               :type ft
                               :default (%table-get f "default")
                               :default-bound-p (nth-value 1 (gethash "default" f))
                               :aliases (%as-list (%table-get f "aliases")))))
                          (%as-list (%table-get source "fields"))))
            schema))
         ((equal type-name "enum")
          (let* ((name (%table-get source "name"))
                 (full (%fullname name ns))
                 (schema (%make-avro-schema :kind :enum
                                            :name name
                                            :namespace ns
                                            :fullname full
                                            :symbols (mapcar #'identity
                                                             (%as-list (%table-get source "symbols")))
                                            :default (%table-get source "default")
                                            :aliases (%as-list (%table-get source "aliases")))))
            (when full (setf (gethash full names) schema))
            schema))
         ((equal type-name "array")
          (%make-avro-schema :kind :array
                             :items (%compile-schema (%table-get source "items") names ns)))
         ((equal type-name "map")
          (%make-avro-schema :kind :map
                             :values (%compile-schema (%table-get source "values") names ns)))
         ((equal type-name "fixed")
          (let* ((name (%table-get source "name"))
                 (full (%fullname name ns))
                 (schema (%make-avro-schema :kind :fixed
                                            :name name
                                            :namespace ns
                                            :fullname full
                                            :size (%table-get source "size"))))
            (when full (setf (gethash full names) schema))
            schema))
         ((stringp type-name)
          (%compile-schema type-name names ns))
         (t
          (error 'avro-schema-error
                 :message (format nil "unrecognized Avro schema type ~S" type))))))
    (t
     (error 'avro-schema-error
            :message (format nil "cannot parse Avro schema from ~S" (type-of source))))))

(defun parse-schema (source)
  "Parse an Avro schema from a JSON string, hash-table, or already-compiled schema."
  (cond
    ((avro-schema-p source) source)
    ((stringp source)
     (multiple-value-bind (json next) (parse-json source)
       (declare (ignore next))
       (%compile-schema json (make-hash-table :test #'equal) nil)))
    (t
     (%compile-schema source (make-hash-table :test #'equal) nil))))

(defun %schema-json (schema)
  (ecase (avro-schema-kind schema)
    ((:null :boolean :int :long :float :double :bytes :string)
     (string-downcase (symbol-name (avro-schema-kind schema))))
    (:union
     (map 'vector #'%schema-json (avro-schema-branches schema)))
    (:array
     (let ((ht (make-hash-table :test #'equal)))
       (setf (gethash "type" ht) "array"
             (gethash "items" ht) (%schema-json (avro-schema-items schema)))
       ht))
    (:map
     (let ((ht (make-hash-table :test #'equal)))
       (setf (gethash "type" ht) "map"
             (gethash "values" ht) (%schema-json (avro-schema-values schema)))
       ht))
    (:enum
     (let ((ht (make-hash-table :test #'equal)))
       (setf (gethash "type" ht) "enum"
             (gethash "name" ht) (avro-schema-name schema)
             (gethash "symbols" ht) (coerce (avro-schema-symbols schema) 'vector))
       ht))
    (:fixed
     (let ((ht (make-hash-table :test #'equal)))
       (setf (gethash "type" ht) "fixed"
             (gethash "name" ht) (avro-schema-name schema)
             (gethash "size" ht) (avro-schema-size schema))
       ht))
    (:record
     (let ((ht (make-hash-table :test #'equal)))
       (setf (gethash "type" ht) "record"
             (gethash "name" ht) (avro-schema-name schema)
             (gethash "fields" ht)
             (map 'vector
                  (lambda (f)
                    (let ((fh (make-hash-table :test #'equal)))
                      (setf (gethash "name" fh) (avro-field-name f)
                            (gethash "type" fh) (%schema-json (avro-field-type f)))
                      (when (avro-field-default-bound-p f)
                        (setf (gethash "default" fh) (avro-field-default f)))
                      fh))
                  (avro-schema-fields schema)))
       ht))))

(defun schema-json (schema)
  "Return the Avro schema as a Lisp JSON tree (hash-tables / vectors / strings)."
  (%schema-json (parse-schema schema)))
