;;; EDN Reader
;;; Edwin Watkeys
;;; edw@poseur.com
;;; 05 MAY 2020

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

(define-record-type <edn-nil>
  (make-edn-nil)
  edn-nil?)

(define edn-nil (make-edn-nil))

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
            ("nil" edn-nil)
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

  (edn-actual-value ((: "#" (-> t ,symbol) (-> v ,edn-value))
                     (make-edn-tagged-value t v))
                    ((: (=> e ,edn-atom)) e)
                    ((: (=> e ,edn-collection)) e))

  (edn-value ((: ,space (-> v ,edn-actual-value)) v)
             ((-> v ,edn-actual-value) v)))

(define (parse-edn str)
  (parse edn-value str))
