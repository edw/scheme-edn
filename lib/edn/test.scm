;;; EDN Reader
;;; Edwin Watkeys
;;; edw@poseur.com
;;; 05 MAY 2020

(define-syntax test-edn
  (syntax-rules ()
    ((_ expect str) (test expect (parse-edn str)))))

(define (run-tests)
  (test-group
   "EDN Tests"
   (test-edn 'nil "nil")

   (test-edn (make-edn-map '((#t #f))) "{true false}")

   (test-edn (make-edn-map '()) "{,}")
   (test-edn (make-edn-map '()) "{}")

   (test-edn (make-edn-map `((,(make-edn-symbol #f "not-key")
                              ,(make-edn-symbol #f "not-value"))))
             "{not-key not-value}")

   (test-edn (make-edn-map '()) "{#_not-key #_ not-value}")
   (test-edn 121 "#_not-key #_ not-value 121")
   (test-edn (make-edn-map '()) "{ , , ,,,,   }")

   (test-edn (make-edn-map '((42 0)))
             "{  42 0 }")

   (test-edn (make-edn-map '((0 1) (2 3)))
             "{  0, 1,,,, 2   3 }")

   (test-edn (make-edn-map '((0 1) (2 3)))
             "{  0, 1,,,, ; what'a up with those?\n 2   3 }")

   (test-edn (make-edn-set '(0))
             "#{0}")

   (test-edn (make-edn-set '(0 1 2))
             "#{0, 1, 2}")

   (test-edn (make-edn-set `(,(make-edn-keyword #f "foo")
                             #(0 1)))
             "#{:foo, [0 1]}")

   (test-edn #(1 2 3 4) "[1 2 ; testing \n3 4]")
   (test-edn #(101) "[#_ 32 101 ] ")
   (test-edn #() "[#_ 32 ] ")
   (test-edn #() "[#_32 ] ")

   (test-edn (make-edn-set '()) "#{#_ 32 }")
   (test-edn (make-edn-set '()) "#{#_32 }")

   (test-edn 42 " 42")
   (test-edn 42 "#_0 42")
   (test-edn 42 "#_1 42")
   (test-edn 42 " +42")
   (test-edn -42 " -42")
   (test-edn 42/9 "-42/-9")
   (test-edn 42/9 "420/90")

   (test-edn (make-edn-big-decimal '("-2" #f #f)) "#_foo -2M")
   (test-edn (make-edn-big-decimal '("2" #f #f)) "#_foo +2M")
   (test-edn (make-edn-big-decimal '("2" #f #f)) "#_foo 2M")
   (test-edn (make-edn-big-decimal '("-29" #f #f)) "#_foo -29M")

   (test-edn 42 "#_foo/bar 42")

   (test-edn 1.0 "#_ #_ #_ #_ -1 [] {} (a b c) 1.0")

   (test-edn 1.0 "1.0")
   (test-edn 10.0 "100.0e-1")
   (test-edn 1.0 "#_foo 1.0")
   (test-edn (make-edn-tagged-value (make-edn-symbol "foo" "bar") 1.0)
             "#foo/bar 1.0")
   (test-edn (make-edn-tagged-value (make-edn-symbol "foo" "bar") 42)
             "#foo/bar #_ 1.0 42")

   (test-edn "" "\"\"")
   (test-edn "foo" "\"foo\"")
   (test-edn "foo\nbar" "\"foo\nbar\"")
   (test-edn "foo\nbar" "\"foo\\nbar\"")

   (test-edn (make-edn-inst "1985-04-12T23:20:50.52Z")
             "#inst \"1985-04-12T23:20:50.52Z\"")

   (test-edn (make-edn-uuid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6")
             "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")

   (test-assert
    (let ((uuid (parse-edn "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"")))
      (and (edn-uuid? uuid)
           (equal? (edn-uuid-value uuid)
                   "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"))))))
