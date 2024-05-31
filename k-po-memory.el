;;; k-po-memory.el --- Translation memory -*- lexical-binding: t -*-

;;; Commentary:

;; Trying to implement a translation memory feature.

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(require 'k-po-entry)

;; FIXME: the dependency graph needs fixing
(declare-function k-po-current-entry "k-po-mode")

(defun k-po-memory--file ()
  "Return the path to the DB file."
  (expand-file-name "k-po-memory.db" user-emacs-directory))

(defvar k-po-memory--connection nil
  "This holds the connection to the `k-po-memory' database.
To read this value, use `k-po-memory--db' instead.")
(defvar k-po-memory--in-transaction nil
  "Whether there is a transaction going on for `k-po-memory' or not.")
(defconst k-po-memory--version 0)
(defconst k-po-memory--schemata
  '((mapping
     ;; INTEGER PRIMARY KEY is alias for the implicit ROWID, except it won't
     ;; change when vacuuming.
     ;; https://www.sqlite.org/lang_vacuum.html#how_vacuum_works
     "id integer primary key"
     ;; We could dedupe this later.
     "source text"
     "target text"
     "file text")))

(defmacro k-po-memory--with-transaction (&rest body)
  "Run BODY in a transaction for the `k-po-memory' database.
Also sets `k-po-memory--in-transaction'."
  (declare (indent 0))
  `(with-sqlite-transaction (canrylog-db)
     (let ((k-po-memory--in-transaction t))
       ,@body)))

(defun k-po-memory--execute (sql &rest args)
  "Run `sqlite-execute' with SQL and ARGS on the cache database.
If SQL is a list, join them with newlines."
  (when (listp sql)
    (setq sql (string-join sql "\n")))
  (let ((db (k-po-memory--db)))
    (prog1 (sqlite-execute db sql args)
      (unless k-po-memory--in-transaction
        (sqlite-execute db "VACUUM;")))))
(defun k-po-memory--select (sql &rest args)
  "Run `sqlite-select' with SQL and ARGS on the cache database.
If SQL is a list, join them with newlines."
  (when (listp sql)
    (setq sql (string-join sql "\n")))
  (sqlite-select (k-po-memory--db) sql args))
(defun k-po-memory--insert (table values)
  "Insert (or replace) VALUES into TABLE.
TABLE can be specified as a symbol.
VALUES is a list of rows. Each row is a list of values
for each column."
  (when (> (length values) 0)
    (apply
     #'k-po-memory--execute
     ;; The statement is "insert into tbl values (?,?,?), (?,?,?) ..."
     ;; This is constructing the placeholders.
     ;; Then we pass in flattened arguments, and this works.
     (format "insert or replace into \"%s\" values %s"
             table
             (mapconcat
              (lambda (row)
                (format "(%s)"
                        (string-join
                         (make-list (length row) "?")
                         ",")))
              values
              ","))
     (flatten-tree values))))

(defun k-po-memory--migrate (old &optional new)
  "Migrate from OLD version to NEW version."
  (cl-block nil
    (when (eql old k-po-memory--version)
      (cl-return k-po-memory--version))
    (unless new
      (setq new (1+ old)))
    ;; (pcase (list old new)
    ;;   ('(1 2)
    ;;    (k-po-memory--execute "drop table goals_old")))
    new))
(cl-defun k-po-memory--db ()
  "Return an open database, initializing if necessary."
  (when k-po-memory--connection
    (cl-return-from k-po-memory--db k-po-memory--connection))
  (let* ((db-file (k-po-memory--file))
         (should-init (not (file-exists-p db-file)))
         db)
    (when should-init
      (make-directory (file-name-directory db-file) t))
    (setq k-po-memory--connection (sqlite-open db-file))
    (setq db k-po-memory--connection)
    (when should-init
      (sqlite-pragma db "foreign_keys = 1")
      (sqlite-pragma db (format "user-version = %s" k-po-memory--version))
      (with-sqlite-transaction db
        (pcase-dolist (`(,tbl . ,schemata) k-po-memory--schemata)
          (sqlite-execute
           db
           (format "CREATE TABLE \"%s\" (%s);"
                   tbl
                   (string-join schemata ","))))))
    (let ((current-version (caar (sqlite-select db "pragma user_version"))))
      (unless (eql current-version k-po-memory--version)
        (message
         "Current memory version %s is out of date, performing backup then migrating..."
         current-version)
        (copy-file db-file
                   (format "%s.%s.bak"
                           db-file
                           (format-time-string "%+4Y%m%dT%H%M%S%z"))
                   :ok)
        (catch 'done
          (while t
            (setq current-version (k-po-memory--migrate current-version))
            (when (eql current-version k-po-memory--version)
              (throw 'done t))
            (when (> current-version k-po-memory--version)
              (error "Somehow migrated to a nonexistant version"))))
        (message
         "Migration complete")))
    db))

(defun k-po-memory-insert-entry (entry file)
  "Insert ENTRY from FILE into the translation memory."
  (k-po-memory--execute
   "insert or replace into \"mapping\" (source,target,file) values (?,?,?)"
   (k-po-entry-msgid entry)
   (k-po-entry-msgstr entry)
   file))

(defun k-po-memory-insert-current-entry ()
  "Insert the source and target text of the entry at point into the memory."
  (k-po-memory-insert-entry
   (k-po-current-entry)
   (buffer-file-name)))

(provide 'k-po-memory)

;;; k-po-memory.el ends here
