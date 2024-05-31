;;; k-po-extract.el --- Dealing with the PO file buffer -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

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
  (with-temp-buffer
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
  (with-temp-buffer
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
