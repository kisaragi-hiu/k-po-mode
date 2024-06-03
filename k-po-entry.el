;;; k-po-entry.el --- Entry object -*- lexical-binding: t -*-

;;; Commentary:

;; Functions for the entry object type.

;;; Code:

(require 'cl-lib)
(require 'k-po-extract)

(cl-defstruct (k-po-entry (:copier nil)
                          (:constructor k-po-entry))
  "An object representing an entry in a PO file."
  start msgctxt-start msgid-start msgid_plural-start msgstr-block-start
  msgstr-form-start msgstr-form-end
  end
  (type nil :documentation "The type of an entry.

Possible values: `obsolete', `fuzzy', `untranslated', or `translated'."))

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
