;;; k-po-memory.el --- Translation memory -*- lexical-binding: t -*-

;;; Commentary:

;; Trying to implement a translation memory feature.

;;; Code:

(require 'cl-lib)

(defun k-po-memory--file ()
  "Return the path to the DB file."
  (expand-file-name "k-po-memory.db" user-emacs-directory))

(defvar k-po-memory--connection nil)
(defconst k-po-memory--version 0)
(defconst k-po-memory--schemata
  '((mapping
     ;; INTEGER PRIMARY KEY is alias for the implicit ROWID, except it won't
     ;; change when vacuuming.
     ;; https://www.sqlite.org/lang_vacuum.html#how_vacuum_works
     "id integer primary key"
     ;; We could dedupe this later.
     "source text"
     "target text")))

;; (cl-defun k-po-memory--db ()
;;   "Return an open database, initializing if necessary."
;;   (when k-po-memory--connection
;;     (cl-return-from k-po-memory--db k-po-memory--connection))
;;   (let ((should-init (not (file-exists-p (k-po-memory--file))))
;;         db)))

(defun k-po-memory-insert ())

(provide 'k-po-memory)

;;; k-po-memory.el ends here
