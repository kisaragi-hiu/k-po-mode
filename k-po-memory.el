;;; k-po-memory.el --- Translation memory -*- lexical-binding: t -*-

;;; Commentary:

;; Trying to implement a translation memory feature.

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(require 'k-po-entry)

;; FIXME: the dependency graph needs fixing
(declare-function k-po-current-entry "k-po-mode")
(declare-function k-po-map-entries "k-po-mode")

(defun k-po-memory--file ()
  "Return the path to the DB file."
  (expand-file-name "k-po-memory.db" user-emacs-directory))

(defvar k-po-memory--connection nil
  "This holds the connection to the `k-po-memory' database.
To read this value, use `k-po-memory--db' instead.")
(defvar k-po-memory--in-transaction nil
  "Whether there is a transaction going on for `k-po-memory' or not.")
(defconst k-po-memory--version 1)
(defconst k-po-memory--schemata
  '((mapping
     ;; INTEGER PRIMARY KEY is alias for the implicit ROWID, except it won't
     ;; change when vacuuming.
     ;; https://www.sqlite.org/lang_vacuum.html#how_vacuum_works
     "id integer primary key"
     ;; We could dedupe this later.
     "source text"
     "target text"
     "file text"
     "source_lang text"
     "target_lang text")))

(defmacro k-po-memory--with-transaction (&rest body)
  "Run BODY in a transaction for the `k-po-memory' database.
Also sets `k-po-memory--in-transaction'."
  (declare (indent 0))
  `(with-sqlite-transaction (k-po-memory--db)
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
    (pcase (list old new)
      ('(0 1)
       (k-po-memory--execute "ALTER TABLE mapping ADD COLUMN source_lang TEXT;")
       (k-po-memory--execute "ALTER TABLE mapping ADD COLUMN target_lang TEXT;")))
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
                   (string-join schemata ",")))))
      ;; Index
      (sqlite-execute db "CREATE INDEX idx_mapping_source ON mapping(source);")
      (sqlite-execute db "CREATE INDEX idx_mapping_file ON mapping(file);"))
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

(defun k-po-memory-insert-entry (entry file source-lang target-lang)
  "Insert ENTRY from FILE into the translation memory.
SOURCE-LANG and TARGET-LANG are languages of the source text and
target text, respectively."
  (k-po-memory--execute
   "insert or replace
into \"mapping\" (source,target,file,source_lang,target_lang)
values (?,?,?,?,?)"
   (k-po-entry-msgid entry)
   (k-po-entry-msgstr entry)
   file
   source-lang
   target-lang))

(defun k-po-memory--insert-current-file (&optional silence)
  "Insert every entry from the current file into the translation memory.
If SILENCE, don\\='t show the progress reporter."
  (k-po-memory--execute
   "delete from mapping where file = ?"
   (buffer-file-name))
  (k-po-map-entries
   (lambda (entry)
     (when (and (not (k-po-entry-header? entry))
                (k-po-entry-type? entry 'translated))
       (let ((source-lang (k-po-current-source-language))
             (target-lang (k-po-current-target-language)))
         (k-po-memory-insert-entry entry (buffer-file-name) source-lang target-lang))))
   (unless silence
     (make-progress-reporter "Inserting translation memory..." 1 (point-max)))))

(defun k-po-memory--insert-file (file)
  "Insert every entry within FILE into the translation memory."
  (with-temp-buffer
    (insert-file-contents file)
    ;; Many functions rely on this to get the current file name
    (setq buffer-file-name (file-truename file))
    (unwind-protect
        (let ((inhibit-message t))
          (k-po-memory--insert-current-file :silence))
      ;; Do not ever ask me to save the temporary buffer
      (setq buffer-file-name nil))))

(defun k-po-memory-insert-dir (dir)
  "Insert every .po file within DIR into the translation memory."
  (interactive "DPO Directory: ")
  (message "Retrieving files...")
  (let* ((files (directory-files-recursively dir (rx ".po" eos) nil nil t))
         (i 0)
         (total (length files)))
    (message "Collecting translation memory...")
    (k-po-memory--with-transaction
      (dolist (file files)
        (cl-incf i)
        (message "Collecting translation memory (%s/%s)... (%s)"
                 i total
                 (file-relative-name file dir))
        (k-po-memory--insert-file file)))
    (message "Collecting translation memory...done")))

(defun k-po-memory-insert-current-file ()
  "Insert entries from the current file into TM.
The difference with `k-po-memory--insert-current-file' is that
this is wrapped in a transaction and is interactive."
  (interactive)
  (k-po-memory--with-transaction
    (k-po-memory--insert-current-file nil)))

(defun k-po-memory-clear ()
  "Clear the translation memory."
  (interactive)
  (k-po-memory--execute "delete from mapping"))

;; We do the counting in Emacs Lisp because we need to use WHERE to take
;; advantage of the index. Using the count aggregate function, GROUP BY, and
;; HAVING seems to make it necessary for SQLite to scan through every row.
(defun k-po-memory--rows-count-group (rows)
  "Group ROWS by count, and sort appropriately."
  (let ((table (make-hash-table :test #'equal))
        ret)
    (pcase-dolist (row rows)
      (if (gethash row table)
          (cl-incf (gethash row table))
        (puthash row 1 table)))
    (maphash
     (lambda (row count)
       (push (list (car row) (cadr row) count) ret))
     table)
    (sort ret (lambda (a b)
                (> (elt a 2)
                   (elt b 2))))))

(defun k-po-memory-get (msgid target-lang)
  "Return the target texts for MSGID.
The value is a list of rows, where each row is (SOURCE TARGET COUNT)."
  (k-po-memory--rows-count-group
   (k-po-memory--select
    "SELECT source, target
FROM mapping
WHERE source = ? AND target_lang = ?"
    msgid target-lang)))

(defun k-po-memory-get-prefix (prefix)
  "Return the target texts whose source text starts with PREFIX.
The value is a list of rows, where each row is (SOURCE TARGET COUNT)."
  (k-po-memory--rows-count-group
   (k-po-memory--select
    "SELECT source, target
FROM mapping
WHERE instr(source, ?) = 1"
    prefix)))

(defun k-po-memory--get-files (source target)
  "Return the files who have mapped SOURCE to TARGET."
  (mapcar #'car
          (k-po-memory--select
           "SELECT DISTINCT file FROM mapping WHERE source = ? AND target = ?"
           source target)))

(provide 'k-po-memory)

;;; k-po-memory.el ends here
