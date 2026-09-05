(in-package #:schema-protocol-avro)

(defvar *generated-package* (find-package '#:schema-protocol-avro.generated))

(defstruct compile-ctx
  (package *generated-package*)
  (filled (make-hash-table :test #'eq)))

(defun %sanitize (string)
  (let ((s (substitute #\- #\_ (substitute #\- #\Space (string string)))))
    (if (plusp (length s))
        (string-upcase s)
        "SCHEMA")))

(defun %name-symbol (name ctx)
  (etypecase name
    (symbol
     (if (eq (symbol-package name) (compile-ctx-package ctx))
         name
         (intern (symbol-name name) (compile-ctx-package ctx))))
    (string (intern (%sanitize name) (compile-ctx-package ctx)))))

(defun %ht-get (table key)
  (and (hash-table-p table) (gethash key table)))

(defun %as-list (value)
  (cond
    ((null value) nil)
    ((and (vectorp value) (not (stringp value))) (coerce value 'list))
    ((listp value) value)
    (t (list value))))

(defun %union-p (node)
  (or (and (vectorp node) (not (stringp node)))
      (and (listp node) (not (hash-table-p node)))))

(defun %null-type-p (node)
  (or (equal node "null")
      (eq node :null)
      (and (stringp node) (string-equal node "null"))
      (and (symbolp node) (string-equal (symbol-name node) "null"))
      (and (hash-table-p node)
           (let ((ty (%ht-get node "type")))
             (and ty (string-equal (if (symbolp ty) (symbol-name ty) ty) "null")
                  (not (%ht-get node "fields")))))))

(defun %type-name (node)
  (cond
    ((stringp node) (string-downcase node))
    ((symbolp node) (string-downcase (symbol-name node)))
    ((hash-table-p node)
     (let ((ty (%ht-get node "type")))
       (cond
         ((stringp ty) (string-downcase ty))
         ((symbolp ty) (string-downcase (symbol-name ty)))
         (t ty))))
    (t nil)))

(defun %tree-from-source (source)
  (cond
    ((avro-protocol:avro-schema-p source)
     (avro-protocol:schema-json source))
    ((stringp source)
     (avro-protocol:schema-json (avro-protocol:parse-schema source)))
    (t source)))

(defun type-from-avro (node ctx &key name-hint)
  (cond
    ((%union-p node)
     (let* ((branches (%as-list node))
            (others (remove-if #'%null-type-p branches)))
       (cond
         ((and (find-if #'%null-type-p branches) (= 1 (length others)))
          `(or :null ,(type-from-avro (first others) ctx :name-hint name-hint)))
         ((= 1 (length branches))
          (type-from-avro (first branches) ctx :name-hint name-hint))
         (t
          `(or ,@(mapcar (lambda (b)
                           (type-from-avro b ctx :name-hint name-hint))
                         branches))))))
    ((or (stringp node) (symbolp node))
     (let ((ty (%type-name node)))
       (cond
         ((string= ty "null") :null)
         ((string= ty "boolean") 'boolean)
         ((or (string= ty "int") (string= ty "long")) 'integer)
         ((string= ty "float") 'float)
         ((string= ty "double") 'number)
         ((string= ty "bytes") '(vector (unsigned-byte 8)))
         ((string= ty "string") 'string)
         (t t))))
    ((hash-table-p node)
     (let ((ty (%type-name node)))
       (cond
         ((or (equal ty "record") (%ht-get node "fields"))
          (let ((n (or (%ht-get node "name") name-hint
                       (gentemp "RECORD" (compile-ctx-package ctx)))))
            (%fill-record ctx n node)
            (%name-symbol n ctx)))
         ((equal ty "enum")
          `(member ,@(mapcar (lambda (s)
                               (intern (string-upcase (string s)) :keyword))
                             (%as-list (%ht-get node "symbols")))))
         ((equal ty "array")
          `(vector ,(type-from-avro (%ht-get node "items") ctx
                                    :name-hint (and name-hint
                                                    (format nil "~A-item" name-hint)))))
         ((equal ty "map")
          'hash-table)
         ((equal ty "fixed")
          '(vector (unsigned-byte 8)))
         ((member ty '("null" "boolean" "int" "long" "float" "double" "bytes" "string")
                  :test #'equal)
          (type-from-avro ty ctx))
         (t t))))
    (t t)))

(defun %field-names (node)
  (mapcar (lambda (f) (and (hash-table-p f) (%ht-get f "name")))
          (%as-list (%ht-get node "fields"))))

(defun %infer-union-tag (records)
  "Shared field used as :tag. Prefer kind/type/tag, else the first common name."
  (let ((common (reduce (lambda (a b) (intersection a b :test #'equal))
                        (mapcar #'%field-names records))))
    (or (find-if (lambda (n)
                  (and (stringp n)
                       (member n '("kind" "type" "tag") :test #'string-equal)))
                common)
        (find-if #'stringp common))))

(defun %eql-from-default (default)
  (cond
    ((null default) nil)
    ((keywordp default) default)
    ((symbolp default) (intern (symbol-name default) :keyword))
    ((stringp default) (intern (string-upcase default) :keyword))
    (t default)))

(defun %fill-record (ctx name node &key supers tag-field)
  (let ((sym (%name-symbol name ctx)))
    (when (gethash sym (compile-ctx-filled ctx))
      (return-from %fill-record (find-class sym)))
    (setf (gethash sym (compile-ctx-filled ctx)) t)
    (let ((slots '()))
      (dolist (field (%as-list (%ht-get node "fields")))
        (unless (hash-table-p field)
          (error 'avro-schema-error :message "record field must be an object"))
        (let* ((fname (%ht-get field "name"))
               (fsym (%name-symbol fname ctx))
               (default (%ht-get field "default"))
               (eql-tag (and tag-field (stringp fname)
                             (string-equal fname tag-field)
                             (%eql-from-default default)))
               (ftype (if eql-tag
                          `(eql ,eql-tag)
                          (type-from-avro (%ht-get field "type") ctx :name-hint fname)))
               (optional (and (consp ftype) (eq (first ftype) 'or)
                              (member :null (rest ftype))))
               (slot `(:name ,fsym
                       :type ,ftype
                       :initargs (,(intern (symbol-name fsym) :keyword))
                       :readers (,fsym)
                       :writers ((setf ,fsym))
                       :key ,(string fname)
                       :required ,(not optional)
                       :optional ,(and optional t))))
          (when default
            (setf slot (append slot (list :initform default
                                          :initfunction (constantly default)))))
          (push slot slots)))
      (ensure-class sym
                    :metaclass (find-class 'schema-class)
                    :direct-superclasses (or supers (list (find-class 'schema-object)))
                    :direct-slots (nreverse slots)))))

(defun %fill-union (ctx name records)
  (let* ((tag (%infer-union-tag records))
         (wrap (%name-symbol name ctx))
         (tag-sym (and tag (%name-symbol tag ctx))))
    (ensure-class wrap
                  :metaclass (find-class 'schema-class)
                  :direct-superclasses (list (find-class 'schema-object))
                  :direct-slots (if tag
                                    (list `(:name ,tag-sym
                                            :type keyword
                                            :initargs (,(intern (symbol-name tag-sym) :keyword))
                                            :readers (,tag-sym)
                                            :writers ((setf ,tag-sym))
                                            :key ,tag
                                            :required t))
                                    nil)
                  :tag tag-sym)
    (setf (gethash wrap (compile-ctx-filled ctx)) t)
    (dolist (r records)
      (%fill-record ctx
                    (or (%ht-get r "name") (gentemp "VARIANT" (compile-ctx-package ctx)))
                    r
                    :supers (list (find-class wrap))
                    :tag-field tag))
    (find-class wrap)))

(defun compile-schema (source &key name (package *generated-package*) &allow-other-keys)
  "Avro JSON schema (string / hash-table / compiled avro-schema) → schema-class.
   A union of records becomes a tagged wrapper (:tag inferred from a shared field)."
  (let* ((tree (%tree-from-source source))
         (ctx (make-compile-ctx :package package)))
    (cond
      ((%union-p tree)
       (let ((records (remove-if-not #'hash-table-p (%as-list tree))))
         (unless records
           (error 'avro-schema-error :message "union has no record branch"))
         (%fill-union ctx
                      (or name (%ht-get (first records) "name")
                          (gentemp "SCHEMA" package))
                      records)))
      ((hash-table-p tree)
       (let ((root (or name (%ht-get tree "name") (gentemp "SCHEMA" package))))
         (unless (or (equal (%type-name tree) "record") (%ht-get tree "fields"))
           (error 'avro-schema-error
                  :message "Avro parse-schema expects a record (or union of records)"))
         (%fill-record ctx root tree)
         (find-class (%name-symbol root ctx))))
      (t
       (error 'avro-schema-error
              :message (format nil "cannot compile Avro schema from ~S" (type-of source)))))))
