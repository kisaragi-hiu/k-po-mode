;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"))

(setq user-emacs-directory (make-temp-file "test-k-po-mode" t))

(require 'k-po-mode)
(require 'buttercup)

(defmacro test--with-fixture (file &rest body)
  "Run BODY within a buffer that has FILE opened."
  (declare (indent 1))
  (setq file (concat "tests/" file))
  `(progn
     (unless (file-exists-p ,file)
       (error "%s does not exist" ,(file-truename file)))
     (with-current-buffer (let ((inhibit-message t))
                            (find-file-noselect ,file))
       (goto-char (point-min))
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
              "zh_TW")))
  (it "gets basic entry values"
    (test--with-fixture "fixtures/a.po"
      (k-po-next-entry)
      (expect (k-po-current-entry)
              :to-equal
              #s(k-po-entry 524 553 568 nil 582 582 593 593 translated))
      (expect (k-po-entry-msgid (k-po-current-entry))
              :to-equal
              "Hello")
      (expect (k-po-entry-msgstr (k-po-current-entry))
              :to-equal
              "嗨"))))

(describe "memory"
  (it "inserts an entry"
    (expect (k-po-memory-insert-entry
             (k-po-entry :msgid "Hello" :msgstr "哈囉")
             "test.po"
             "en"
             "zh_TW")
            :to-equal 1)
    (expect (k-po-memory-get "Hello" "zh_TW")
            :to-equal
            '(#s(k-po-memory-entry "Hello" "哈囉" 1)))))
