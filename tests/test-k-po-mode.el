;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'k-po-mode)
(require 'buttercup)

(describe "hello"
  (it "says hello"
    (expect (k-po-mode-hello-world)
            :to-equal
            "Hello world!")))
