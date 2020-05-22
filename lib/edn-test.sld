;;; EDN Reader
;;; Edwin Watkeys
;;; edw@poseur.com
;;; 05 MAY 2020

(define-library (edn-test)
  (import (scheme base) (edn) (chibi test))
  (export run-tests)
  (include "edn/test.scm"))
