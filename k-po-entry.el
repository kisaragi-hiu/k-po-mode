;;; k-po-entry.el --- Entry object -*- lexical-binding: t -*-

;;; Commentary:

;; Functions for the entry object type.

;;; Code:

(require 'cl-lib)
(require 'k-po-vars)

;;;; Utils

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

;;;; Entry object
(cl-defstruct (k-po-entry (:copier nil)
                          (:constructor k-po-entry))
  "An object representing an entry in a PO file."
  start msgctxt-start msgid-start msgid_plural-start msgstr-block-start
  msgstr-form-start msgstr-form-end
  end
  (type nil :documentation "The type of an entry.

Possible values: `obsolete', `fuzzy', `untranslated', or `translated'."))

;;;; Creating entries from buffer in practice

(defun k-po-find-span-of-entry ()
  "Find the extent of the PO file entry where the cursor is.
Set variables k-po-start-of-entry, k-po-start-of-msgctxt, k-po-start-of-msgid,
k-po-start-of-msgid_plural, k-po-start-of-msgstr-block, k-po-end-of-entry, and
k-po-entry-type to meaningful values. k-po-entry-type may be set to: obsolete,
fuzzy, untranslated, or translated."
  (let ((here (point)))
    (if (re-search-backward k-po-any-msgstr-block-regexp nil t)
        (progn
          ;; After a backward match, (match-end 0) will not extend
          ;; beyond point, in case point was *inside* the regexp.  We
          ;; need a dependable (match-end 0), so we redo the match in
          ;; the forward direction.
          (re-search-forward k-po-any-msgstr-block-regexp)
          (if (<= (match-end 0) here)
              (progn
                ;; We most probably found the msgstr of the previous
                ;; entry.  The current entry then starts just after
                ;; its end, save this information just in case.
                (setq k-po-start-of-entry (match-end 0))
                ;; However, it is also possible that we are located in
                ;; the crumb after the last entry in the file.  If
                ;; yes, we know the middle and end of last PO entry.
                (setq k-po-start-of-msgstr-block (match-beginning 0)
                      k-po-end-of-entry (match-end 0))
                (if (re-search-forward k-po-any-msgstr-block-regexp nil t)
                    (progn
                      ;; We definitely were not in the crumb.
                      (setq k-po-start-of-msgstr-block (match-beginning 0)
                            k-po-end-of-entry (match-end 0)))
                  ;; We were in the crumb.  The start of the last PO
                  ;; file entry is the end of the previous msgstr if
                  ;; any, or else, the beginning of the file.
                  (goto-char k-po-start-of-msgstr-block)
                  (setq k-po-start-of-entry
                        (if (re-search-backward k-po-any-msgstr-block-regexp nil t)
                            (match-end 0)
                          (point-min)))))
            ;; The cursor was inside msgstr of the current entry.
            (setq k-po-start-of-msgstr-block (match-beginning 0)
                  k-po-end-of-entry (match-end 0))
            ;; The start of this entry is the end of the previous
            ;; msgstr if any, or else, the beginning of the file.
            (goto-char k-po-start-of-msgstr-block)
            (setq k-po-start-of-entry
                  (if (re-search-backward k-po-any-msgstr-block-regexp nil t)
                      (match-end 0)
                    (point-min)))))
      ;; The cursor was before msgstr in the first entry in the file.
      (setq k-po-start-of-entry (point-min))
      (goto-char k-po-start-of-entry)
      ;; There is at least the PO file header, so this should match.
      (re-search-forward k-po-any-msgstr-block-regexp)
      (setq k-po-start-of-msgstr-block (match-beginning 0)
            k-po-end-of-entry (match-end 0)))
    ;; Find start of msgid.
    (goto-char k-po-start-of-entry)
    (re-search-forward k-po-any-msgctxt-msgid-regexp)
    (setq k-po-start-of-msgctxt (match-beginning 0))
    (goto-char k-po-start-of-entry)
    (re-search-forward k-po-any-msgid-regexp)
    (setq k-po-start-of-msgid (match-beginning 0))
    (save-excursion
      (goto-char k-po-start-of-msgid)
      (setq k-po-start-of-msgid_plural
            (if (re-search-forward k-po-any-msgid_plural-regexp
                                   k-po-start-of-msgstr-block t)
                (match-beginning 0)
              nil)))
    (save-excursion
      (when (>= here k-po-start-of-msgstr-block)
        ;; point was somewhere inside of msgstr*
        (goto-char here)
        (end-of-line)
        (re-search-backward "^\\(#~[ \t]*\\)?msgstr"))
      ;; Detect the boundaries of the msgstr we are interested in.
      (re-search-forward k-po-any-msgstr-form-regexp)
      (setq k-po-start-of-msgstr-form (match-beginning 0)
            k-po-end-of-msgstr-form (match-end 0)))
    ;; Classify the entry.
    (setq k-po-entry-type
          (if (eq (following-char) ?#)
              'obsolete
            (goto-char k-po-start-of-entry)
            (if (re-search-forward k-po-fuzzy-regexp k-po-start-of-msgctxt t)
                'fuzzy
              (goto-char k-po-start-of-msgstr-block)
              (if (looking-at k-po-untranslated-regexp)
                  'untranslated
                'translated))))
    ;; Put the cursor back where it was.
    (goto-char here)))

(defun k-po--entry-from-vars ()
  "Return a `k-po-entry' from the variables set by `k-po-find-span-of-entry'."
  (k-po-entry :start k-po-start-of-entry
              :msgctxt-start k-po-start-of-msgctxt
              :msgid-start k-po-start-of-msgid
              :msgid_plural-start k-po-start-of-msgid_plural
              :msgstr-block-start k-po-start-of-msgstr-block
              :msgstr-form-start k-po-start-of-msgstr-form
              :msgstr-form-end k-po-end-of-msgstr-form
              :end k-po-end-of-entry
              :type k-po-entry-type))

(defun k-po-current-entry ()
  "Return the current entry at point, as an entry object."
  (k-po-find-span-of-entry)
  (k-po--entry-from-vars))

;;;; Extra entry properties

(defun k-po-entry-msgid (entry)
  "Return the msgid text of ENTRY."
  (k-po-extract-unquoted (current-buffer)
                         (k-po-entry-msgid-start entry)
                         (or (k-po-entry-msgid_plural-start entry)
                             (k-po-entry-msgstr-block-start entry))))

(defun k-po-entry-msgid_plural (entry)
  "Return the unquoted msgid_plural string from ENTRY.
Return nil if it is not present."
  (when (k-po-entry-msgid_plural-start entry)
    (k-po-extract-unquoted (current-buffer)
                           (k-po-entry-msgid_plural-start entry)
                           (k-po-entry-msgstr-block-start entry))))

(defun k-po-entry-msgstr-flavor (entry)
  "Helper function to detect msgstr and msgstr[] variants of ENTRY.
Returns one of \"msgstr\" or \"msgstr[i]\" for some i."
  (save-excursion
    (goto-char (k-po-entry-msgstr-form-start entry))
    (re-search-forward "^\\(#~[ \t]*\\)?\\(msgstr\\(\\[[0-9]\\]\\)?\\)")
    (match-string 2)))

(defun k-po-entry-msgstr (entry)
  "Extract and return the unquoted msgstr string for ENTRY."
  (k-po-extract-unquoted (current-buffer)
                         (k-po-entry-msgstr-form-start entry)
                         (k-po-entry-msgstr-form-end entry)))

(defun k-po-entry-comment (entry)
  "Return the editable comment of ENTRY in the uncommented form."
  (let ((buffer (current-buffer))
        (obsolete (k-po-entry-type? entry 'obsolete)))
    (save-excursion
      (goto-char (k-po-entry-start entry))
      (if (re-search-forward k-po-comment-regexp (k-po-entry-end entry) t)
          (with-temp-buffer
            (insert-buffer-substring buffer (match-beginning 0) (match-end 0))
            (goto-char (point-min))
            (while (not (eobp))
              (if (looking-at (if obsolete "#\\(\n\\| \\)" "# ?"))
                  (replace-match "" t t))
              (forward-line 1))
            (buffer-string))
        ""))))

(defun k-po-entry-header? (entry)
  "Return whether ENTRY is the header entry."
  (or (eql 1 (k-po-entry-start entry))
      (equal "" (k-po-entry-msgid entry))))

(defun k-po-entry-type? (entry type)
  "Return whether ENTRY is of TYPE.
TYPE can be `obsolete', `fuzzy', `untranslated', or `translated'."
  (eq (k-po-entry-type entry) type))

(provide 'k-po-entry)

;;; k-po-entry.el ends here
