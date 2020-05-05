;;; EDN Reader
;;; Edwin Watkeys
;;; edw@poseur.com
;;; 05 MAY 2020

(define-library (edn)
  (import (scheme base) (scheme char) (chibi parse))
  (export parse-edn edn-value)
  (include "edn/edn.scm"))
