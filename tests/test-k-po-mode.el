;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "k-po*.el"
              (:report-format 'lcov)
              (:send-report nil)))

(require 'k-po-mode)
(require 'buttercup)

(describe "hello"
  (it "says hello"
    (expect (k-po-mode-hello-world)
            :to-equal
            "Hello world!")))
