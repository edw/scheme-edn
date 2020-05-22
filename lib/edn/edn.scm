;; edn.scm - EDN Reading
;; Copyright (c) 2020 Edwin Watkeys.  All rights reserved.
;; MIT-licensed
;; https://github.com/edw/scheme-edn

;;> EDN is a data format from the Clojure ecosystem. It is essentially
;;> the format of Clojure programs, minus surface syntax such as
;;> quoting, syntax-quoting, and @ rest argument syntax.

;;> This reader depends on the (chibi parse) module, part of Chibi
;;> Scheme but theoretically portable to any R7RS Scheme.

;;> This library, (edn), exports parse-edn and edn-value. Parse-edn is
;;> procedure of one argument and parses the passed source, returning a
;;> data structure representing the passed EDN. If parsing fails, an error
;;> is raised. Edn-value is a parser combinator that can be called with
;;> call-with-parse or other similar procedures in the (chibi parse)
;;> library.

;;> Additionally, the library exports predicates, accessors, and
;;> constructors for types that don't have exact analogs in Scheme or
;;> where the analogs are not nearly-universally supported. Of
;;> particular note, the library returns EDN nil as the Scheme
;;> symbol nil.

;;> This implementation should parse any valid EDN document. Also,
;;> since Clojure programs may emit ratio values, they are parsed into
;;> Scheme ratios despite their absence from the EDN standard. Finally,
;;> note that parse-edn will return #f on failure, which is also the
;;> result of parsing the string "false". If this is a problem, use the
;;> call-with-parse procedure metioned above.

;;> A test suite is included in the `(edn-test)` library. To execute
;;> it, evaluate `(run-tests)`.

(define (make-float i f e)
  (string->number (string-append i "." f "e" e)))

(define-record-type <edn-sequence>
  (make-edn-sequence els)
  edn-sequence?
  (els edn-sequence-elements))

(define-record-type <edn-map>
  (make-edn-map els)
  edn-map?
  (els edn-map-elements))

(define-record-type <edn-set>
  (make-edn-set els)
  edn-set?
  (els edn-set-elements))

(define-record-type <edn-big-decimal>
  (make-edn-big-decimal els)
  edn-big-decimal?
  (els edn-big-decimal-elements))

(define-record-type <edn-big-integer>
  (make-edn-big-integer els)
  edn-big-integer?
  (els edn-big-integer-elements))

(define-record-type <edn-tagged-value>
  (make-edn-tagged-value tag value)
  edn-tagged-value?
  (tag edn-tagged-value-tag)
  (value edn-tagged-value-value))

(define-record-type <edn-symbol>
  (make-edn-symbol ns name)
  edn-symbol?
  (ns edn-symbol-namespace)
  (name edn-symbol-name))

(define-record-type <edn-inst>
  (make-edn-inst value)
  edn-inst?
  (value edn-inst-value))

(define-record-type <edn-uuid>
  (make-edn-uuid value)
  edn-uuid?
  (value edn-uuid-value))

(define-record-type <edn-keyword>
  (make-edn-keyword ns name)
  edn-keyword?
  (ns edn-keyword-namespace)
  (name edn-keyword-name))

(define-grammar edn

  (symbol-rest ((or ,symbol-first #\# #\: numeric)))
  (symbol-first ((or #\. #\* #\+ #\! #\- #\_ #\? #\$ #\% #\& #\= #\< #\> alphabetic)))
  (symbol ((: (-> ns-first ,symbol-first) (-> ns-rest (* ,symbol-rest))
              #\/ (-> name-first ,symbol-first) (-> name-rest (* ,symbol-rest)))
           (make-edn-symbol (apply string ns-first ns-rest)
                            (apply string name-first name-rest)))
          ((: (-> first ,symbol-first) (-> rest (* ,symbol-rest)))
           (make-edn-symbol #f (apply string first rest))))

  (keyword-rest ((or ,keyword-first #\# #\:)))
  (keyword-first ((or #\. #\* #\+ #\! #\- #\_ #\? #\$ #\% #\& #\= #\< #\> alphabetic numeric)))
  (keyword ((: ":" (-> ns-first ,symbol-first) (-> ns-rest (* ,symbol-rest))
               #\/ (-> name-first ,keyword-first) (-> name-rest (* ,keyword-rest)))
            (make-edn-keyword (apply string ns-first ns-rest)
                              (apply string name-first name-rest)))
           ((: ":" (-> first ,keyword-first) (-> rest (* ,keyword-rest)))
            (make-edn-keyword #f (apply string first rest))))

  (ratio ((-> els (: (-> numer ,fp-int) "/" (-> denom ,fp-int)))
          (/ (string->number numer) (string->number denom))))

  (fp-int ((: "+" (-> first (/ "19")) (-> rest (+ numeric))) (apply string first rest))
          ((: "+" (-> d numeric)) (string d))
          ((: "-" (-> first (/ "19")) (-> rest (+ numeric))) (apply string #\- first rest))
          ((: "-" (-> d numeric)) (string #\- d))
          ((: (-> first (/ "19")) (-> rest (+ numeric))) (apply string first rest))
          ((-> d numeric) (string d)))

  (fp-frac ((: "." (-> ds (+ numeric))) (apply string ds)))

  (fp-exp ((: "e+" (-> ds (+ numeric))) (apply string ds))
          ((: "e-" (-> ds (+ numeric))) (apply string #\- ds))
          ((: "e" (-> ds (+ numeric))) (apply string ds))
          ((: "E+" (-> ds (+ numeric))) (apply string ds))
          ((: "E-" (-> ds (+ numeric))) (apply string #\- ds))
          ((: "E" (-> ds (+ numeric))) (apply string ds)))

  (float ((: (=> ds ,fp-int) "M") (make-edn-big-decimal (list ds #f #f)))
         ((: (-> i ,fp-int) (-> f ,fp-frac) (-> e ,fp-exp) "M")
          (make-edn-big-decimal (list i f e)))
         ((: (-> i ,fp-int) (-> e ,fp-exp) "M") (make-edn-big-decimal (list i #f e)))
         ((: (-> i ,fp-int) (-> f ,fp-frac) "M") (make-edn-big-decimal (list i f #f)))
         ((: (-> i ,fp-int) (-> f ,fp-frac) (-> e ,fp-exp)) (make-float i f e))
         ((: (-> i ,fp-int) (-> e ,fp-exp)) (make-float i ".0" e))
         ((: (-> i ,fp-int) (-> f ,fp-frac)) (make-float i f "0")))

  (number (,float)
          (,ratio)
          ((: (-> s ,fp-int) "N") (make-edn-big-integer s))
          ((-> s ,fp-int) (string->number s)))

  (edn-set ((: "#{" ,space "}") (make-edn-set '()))
           ((: "#{}") (make-edn-set '()))
           ((: "#{" (-> entries (* ,edn-value)) ,space "}")
            (make-edn-set entries))
           ((: "#{" (-> entries (* ,edn-value)) "}")
            (make-edn-set entries)))

  (edn-sequence ((: "(" ,space ")") (make-edn-sequence '()))
                ("()" (make-edn-sequence '()))
                ((: "(" (-> entries (* ,edn-value)) (? ,space) ")")
                 (make-edn-sequence entries)))

  (edn-vector ((: "[" ,space "]") (vector))
              ("[]" (vector))
              ((: "[" (-> entries (* ,edn-value)) (? ,space) "]")
               (apply vector entries)))

  (map-entry ((: (-> key ,edn-value) (-> value ,edn-value))
              (list key value)))

  (edn-map ((: "{" ,space "}") (make-edn-map '()))
           ("{}" (make-edn-map '()))
           ((: "{" (-> entries (* ,map-entry)) ,space "}")
            (make-edn-map entries))
           ((: "{" (-> entries (* ,map-entry)) "}")
            (make-edn-map entries)))

  (hexdigit ((or (/ "09") (/ "af") (/ "AF"))))

  (character ("\\return" #\x0d)
             ("\\newline" #\x0a)
             ("\\space" #\x20)
             ("\\tab" #\x09)
             ("\\\\" #\\)
             ((: "\\u" (-> ds (: ,hexdigit ,hexdigit ,hexdigit ,hexdigit)))
              (integer->char (string->number (apply string ds) 16)))
             ((: "\\" (-> c any)) c))

  (edn-string-char ("\\n" (integer->char 10))
                   ("\\t" (integer->char 9))
                   ("\\r" (integer->char 13))
                   ("\\\\" (integer->char 92))
                   ("\\\"" (integer->char 34))
                   ((-> c any) c))

  (edn-string ("\"\"" "")
              ((: "\"" (-> cs (+ ,edn-string-char)) "\"")
               (list->string cs)))

  (edn-atom ("true" #t)
            ("false" #f)
            ("nil" 'nil)
            ((-> atom (or ,keyword ,number ,symbol ,character ,edn-string)) atom))

  (edn-collection (,edn-sequence)
                  (,edn-set)
                  (,edn-vector)
                  (,edn-map))

  (comment (";\n")
           ((: ";"
               (* ,(parse-char (lambda (ch) (not (= (char->integer ch) 10)))))
               "\n")))

  (discarded-value ((: "#_" ,edn-value)))
  (actual-space ((+ (or ,(parse-char char-whitespace?) ","))))
  (space ((+ (or ,comment ,actual-space ,discarded-value))))

  (edn-actual-value ((: "#inst" (-> v ,edn-value)) (make-edn-inst v))
                    ((: "#uuid" (-> v ,edn-value)) (make-edn-uuid v))
                    ((: "#" (-> t ,symbol) (-> v ,edn-value))
                     (make-edn-tagged-value t v))
                    ((: (=> e ,edn-atom)) e)
                    ((: (=> e ,edn-collection)) e))

  (edn-value ((: ,space (-> v ,edn-actual-value)) v)
             ((-> v ,edn-actual-value) v)))

(define (parse-edn source)
  (call-with-parse edn-value source 0
                   (lambda (result source index fail)
                     result)
                   (lambda e
                     (error e))))
