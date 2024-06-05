;;; k-po-extract.el --- Dealing with the PO file buffer -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'k-po-vars)

;; By reusing the same buffer, we avoid having to repeatedly create and destroy
;; buffers. That appears to be slower than erase-buffer.
;;
;; This also works around Doom's buffer-predicate frame parameter,
;; `doom-buffer-frame-predicate', being run for each buffer when they are
;; killed. The function is quite slow.
(defmacro k-po-with-temp-buffer (&rest body)
  "Run BODY within a temporary buffer and return the value.
Like `with-temp-buffer', but reuses the same temporary buffer."
  `(with-current-buffer (get-buffer-create " *k-po-temp*" t)
     (setq buffer-undo-list t)
     (erase-buffer)
     ,@body))

(defun k-po-goto-header ()
  "Jump to the start of the header, if any.
If a header does not exist, return nil. Otherwise, return
\(TYPE . END), where TYPE is either t or `legacy' (for an
old-style header), and END is the end position of the header."
  (interactive)
  (goto-char (point-min))
  (let (end-of-header start-of-header ret)
    (when (re-search-forward k-po-any-msgstr-block-regexp nil t)
      ;; There is at least one entry.
      (goto-char (match-beginning 0))
      (forward-line -1)
      (setq start-of-header (point))
      (setq end-of-header (match-end 0))
      (when (looking-at "msgid \"\"\n")
        ;; There is indeed a PO file header.
        (if (re-search-forward "\n\"PO-Revision-Date: "
                               end-of-header t)
            (setq ret (cons t end-of-header))
          (setq ret (cons 'legacy end-of-header)))
        (goto-char start-of-header)
        ret))))

(defun k-po-extract-unquoted (buffer start end)
  "Extract and return the unquoted string in BUFFER going from START to END.
Crumb preceding or following the quoted string is ignored."
  (save-excursion
    (goto-char start)
    (search-forward "\"")
    (setq start (point))
    (goto-char end)
    (search-backward "\"")
    (setq end (point)))
  (k-po-extract-part-unquoted buffer start end))

(defun k-po-extract-part-unquoted (buffer start end)
  "Extract and return the unquoted string in BUFFER going from START to END.
Surrounding quotes are already excluded by the position of START and END."
  (k-po-with-temp-buffer
   (insert-buffer-substring buffer start end)
   ;; Glue concatenated strings.
   (goto-char (point-min))
   (while (re-search-forward "\"[ \t]*\\\\?\n\\(#~\\)?[ \t]*\"" nil t)
     (replace-match "" t t))
   ;; Remove escaped newlines.
   (goto-char (point-min))
   (while (re-search-forward "\\\\[ \t]*\n" nil t)
     (replace-match "" t t))
   ;; Unquote individual characters.
   (goto-char (point-min))
   (while (re-search-forward "\\\\[\"abfnt\\0-7]" nil t)
     (cond ((eq (preceding-char) ?\") (replace-match "\"" t t))
           ((eq (preceding-char) ?a) (replace-match "\a" t t))
           ((eq (preceding-char) ?b) (replace-match "\b" t t))
           ((eq (preceding-char) ?f) (replace-match "\f" t t))
           ((eq (preceding-char) ?n) (replace-match "\n" t t))
           ((eq (preceding-char) ?t) (replace-match "\t" t t))
           ((eq (preceding-char) ?\\) (replace-match "\\" t t))
           (t (let ((value (- (preceding-char) ?0)))
                (replace-match "" t t)
                (while (looking-at "[0-7]")
                  (setq value (+ (* 8 value) (- (following-char) ?0)))
                  (replace-match "" t t))
                (insert value)))))
   (buffer-string)))

(defun k-po-call-requoted (func prefix obsolete)
  "Call FUNC, which inserts a string, and return the string fully requoted.
If PREFIX, precede the result with its contents.
If OBSOLETE, comment all generated lines in the returned string.
FUNC should insert the wanted string in the buffer which is
current at the time of evaluation. If FUNC is itself a string,
then this string inserted."
  (k-po-with-temp-buffer
   (if (stringp func)
       (insert func)
     (push-mark)
     (funcall func))
   (goto-char (point-min))
   (let ((multi-line (re-search-forward "[^\n]\n+[^\n]" nil t)))
     (goto-char (point-min))
     (while (re-search-forward "[\"\a\b\f\n\r\t\\]" nil t)
       (cond ((eq (preceding-char) ?\") (replace-match "\\\"" t t))
             ((eq (preceding-char) ?\a) (replace-match "\\a" t t))
             ((eq (preceding-char) ?\b) (replace-match "\\b" t t))
             ((eq (preceding-char) ?\f) (replace-match "\\f" t t))
             ((eq (preceding-char) ?\n)
              (replace-match (if (or (not multi-line) (eobp))
                                 "\\n"
                               "\\n\"\n\"")
                             t t))
             ((eq (preceding-char) ?\r) (replace-match "\\r" t t))
             ((eq (preceding-char) ?\t) (replace-match "\\t" t t))
             ((eq (preceding-char) ?\\) (replace-match "\\\\" t t))))
     (goto-char (point-min))
     (if prefix (insert prefix " "))
     (insert (if multi-line "\"\"\n\"" "\""))
     (goto-char (point-max))
     (insert "\"")
     (if prefix (insert "\n"))
     (if obsolete
         (progn
           (goto-char (point-min))
           (while (not (eobp))
             (or (eq (following-char) ?\n) (insert "#~ "))
             (search-forward "\n"))))
     (buffer-string))))

(provide 'k-po-extract)

;;; k-po-extract.el ends here
