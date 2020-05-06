;;; EDN Reader
;;; Edwin Watkeys
;;; edw@poseur.com
;;; 05 MAY 2020

(define-library (edn)
  (import (scheme base) (scheme char) (chibi parse))
  (export parse-edn edn-value
          edn-nil? edn-nil
          edn-sequence? edn-sequence-elements make-edn-sequence
          edn-map? edn-map-elements make-edn-map
          edn-set? edn-set-elements make-edn-set
          edn-big-decimal? edn-big-decimal-elements make-edn-big-decimal
          edn-big-integer? edn-big-integer-elements make-edn-big-integer

          make-edn-inst make-edn-uuid

          edn-tagged-value? edn-tagged-value-tag edn-tagged-value-value
          make-edn-tagged-value

          edn-symbol? edn-symbol-namespace edn-symbol-name
          make-edn-symbol

          edn-keyword? edn-keyword-namespace edn-keyword-name
          make-edn-keyword)
  (include "edn/edn.scm"))
