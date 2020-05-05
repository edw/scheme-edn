;;; EDN Reader
;;; Edwin Watkeys
;;; edw@poseur.com
;;; 05 MAY 2020

(define-syntax test-edn
  (syntax-rules ()
    ((_ expect str) (test expect (parse-edn str)))))

(test-group "EDN Tests"
            (test-edn '(map ((#t #f))) "{true false}")

            (test-edn '(map ()) "{,}")
            (test-edn '(map ()) "{}")

            (test-edn '(map (((symbol (#f "not-key"))
                              (symbol (#f "not-value")))))
                      "{not-key not-value}")

            (test-edn '(map ()) "{#_not-key #_ not-value}")
            (test-edn 121 "#_not-key #_ not-value 121")
            (test-edn '(map ()) "{ , , ,,,,   }")

            (test-edn '(map ((42 0)))
                      "{  42 0 }")

            (test-edn '(map ((0 1) (2 3)))
                      "{  0, 1,,,, 2   3 }")

            (test-edn '(map ((0 1) (2 3)))
                      "{  0, 1,,,, ; what'a up with those?\n 2   3 }")

            (test-edn '(set (0))
                      "#{0}")

            (test-edn '(set (0 1 2))
                      "#{0, 1, 2}")

            (test-edn '(set ((keyword (#f "foo"))
                             #(0 1)))
                      "#{:foo, [0 1]}")

            (test-edn #(1 2 3 4) "[1 2 ; testing \n3 4]")
            (test-edn #(101) "[#_ 32 101 ] ")
            (test-edn #() "[#_ 32 ] ")
            (test-edn #() "[#_32 ] ")

            (test-edn '(set ()) "#{#_ 32 }")
            (test-edn '(set ()) "#{#_32 }")

            (test-edn 42 " 42")
            (test-edn 42 "#_0 42")
            (test-edn 42 "#_1 42")
            (test-edn 42 " +42")
            (test-edn -42 " -42")
            (test-edn 42/9 "-42/-9")
            (test-edn 42/9 "420/90")

            (test-edn '(big-decimal "-2") "#_foo -2M")
            (test-edn '(big-decimal "2") "#_foo +2M")
            (test-edn '(big-decimal "2") "#_foo 2M")
            (test-edn '(big-decimal "-29") "#_foo -29M")

            (test-edn 42 "#_foo/bar 42")

            (test-edn 1.0 "#_ #_ #_ #_ -1 [] {} (a b c) 1.0")

            (test-edn 1.0 "1.0")
            (test-edn 10.0 "100.0e-1")
            (test-edn 1.0 "#_foo 1.0")
            (test-edn '(tagged-value ((symbol ("foo" "bar")) 1.0))
                      "#foo/bar 1.0")
            (test-edn '(tagged-value ((symbol ("foo" "bar")) 42))
                      "#foo/bar #_ 1.0 42")

            (test-edn "" "\"\"")
            (test-edn "foo" "\"foo\"")
            (test-edn "foo\nbar" "\"foo\nbar\"")
            (test-edn "foo\nbar" "\"foo\\nbar\""))
