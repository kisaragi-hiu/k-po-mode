;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'k-po-mode)
(require 'buttercup)

(describe "language code"
  (it "works"
    (expect (k-po--language->code "English")
            :to-equal
            "en")
    (expect (k-po--language<-code "ja")
            :to-equal
            "Japanese")))
