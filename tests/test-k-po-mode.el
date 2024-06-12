;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'k-po-mode)
(require 'buttercup)

(defmacro test--with-fixture (file &rest body)
  "Run BODY within a buffer that has FILE opened."
  (declare (indent 1))
  (setq file (concat "tests/" file))
  `(progn
     (unless (file-exists-p ,file)
       (error "%s does not exist" ,(file-truename file)))
     (with-current-buffer (find-file-noselect ,file)
       ,@body)))

(describe "language code"
  (it "works"
    (expect (k-po--language->code "English")
            :to-equal
            "en")
    (expect (k-po--language<-code "ja")
            :to-equal
            "Japanese")))

(describe "navigation"
  (it "gets the right language"
    (test--with-fixture "fixtures/a.po"
      (expect (k-po-current-target-language)
              :to-equal
              "zh_TW"))))
