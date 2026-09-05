# avro-protocol

CLOS **Avro** binary encode/decode for [cl-stack](https://github.com/egao1980/cl-stack). Writer schema is required; reader schema is optional (resolution + defaults). Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:avro`.

`schema-protocol-avro` implements [`schema-protocol`](https://github.com/egao1980/schema-protocol) `:avro` (`defschema` ↔ Avro JSON schema tree).

OCI **0.1.0** — `ghcr.io/egao1980/cl-systems/avro-protocol:0.1.0` · `schema-protocol-avro:0.1.1`

```lisp
(asdf:load-system "avro-protocol")   ; nick stack-avro; registers :avro

(let ((schema "{\"type\":\"record\",\"name\":\"User\",\"fields\":[
                 {\"name\":\"id\",\"type\":\"long\"},
                 {\"name\":\"name\",\"type\":\"string\"}]}"))
  (avro-protocol:encode '(:id 1 :name "ada") :schema schema))

(let ((avro-protocol:*avro-schema* "\"string\""))
  (serdes-protocol:encode "hi" :format :avro))

(asdf:load-system "schema-protocol-avro")
(schema-protocol:emit-schema 'user :format :avro)
(schema-protocol:avro-schema 'user)   ; same
(schema-protocol:parse-schema avro-json :format :avro)  ; → schema-class
```

Same Lisp mapping as json-protocol (`:null`, string-key hash-tables, vectors). Unions pick the first matching branch.

## License

MIT — see [LICENSE](LICENSE).
