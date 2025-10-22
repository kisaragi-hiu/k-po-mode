;;; k-po-mode.el --- A po-mode fork that hopefully works better for me -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (f "0.20.0"))
;; Homepage: https://github.com/kisaragi-hiu/k-po-mode
;; Keywords: po languages


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; I don't know why po-mode from
;; https://github.com/emacsmirror/po-mode/ gave me a bunch of errors,
;; so instead of figuring it out I'm just going to patch it so that it
;; works.
;;
;; I'm also interested in adding multi-file project-wide translation features in
;; here.

;;; Code:

(require 'f)
(require 'eieio)

(require 'k-po-vars)

(require 'k-po-entry)
(require 'k-po-sidebar)
(require 'k-po-memory)



;; Handle portable highlighting.  Code has been adapted (OK... stolen! :-)
;; from `ispell.el'.

(defun k-po-create-overlay ()
  "Create and return a deleted overlay structure.
The variable `k-po-highlight-face' selects the face to use for highlighting."
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face k-po-highlight-face)
    ;; The fun thing is that a deleted overlay retains its face, and is
    ;; movable.
    (delete-overlay overlay)
    overlay))

(defun k-po-highlight (overlay start end &optional buffer)
  "Use OVERLAY to highlight the string from START to END.
If limits are not relative to the current buffer, use optional BUFFER."
  (move-overlay overlay start end (or buffer (current-buffer))))

(defun k-po-dehighlight (overlay)
  "Display normally the last string which OVERLAY highlighted.
The current buffer should be in PO mode, when this function is called."
  (delete-overlay overlay))


;;; PO mode variables and constants (usually not to customize).

(defconst k-po-help-display-string
  "\
PO Mode Summary           Next Previous            Miscellaneous
*: Later, /: Docum        n    p    Any type       .     Redisplay
                          t    T    Translated
Moving around             f    F    Fuzzy          ?, h  This help
<    First if any         o    O    Obsolete       =     Current index
>    Last if any          u    U    Untranslated   0     Other window
/SPC Auto select                                   V     Validate
                          Msgstr Comments          M     Mail officially
Modifying entries         RET  #    Call editor
TAB   Remove fuzzy mark   k    K    Kill to        E     Edit out full
DEL   Fuzzy or fade out   w    W    Copy to        Q     Forceful quit
LFD   Init with msgid     y    Y    Yank from      q     Confirm and quit

Position Stack
m  Mark and push current
r  Pop and return
x  Exchange current/top

Program Sources           Lexicography
s    Cycle reference      *l    Lookup translation
M-s  Select reference     *M-l  Add/edit translation
S    Consider path        *L    Consider lexicon
M-S  Ignore path          *M-L  Ignore lexicon
"
  "Help page for PO mode.")

(defconst k-po-mode-menu-layout
  `("PO"
    ("Moving around"
     ["Auto select" k-po-auto-select-entry
      :help "Jump to next interesting entry"]
     "---"
     ;; Forward
     ["Any next" k-po-next-entry
      :help "Jump to next entry"]
     ["Next translated" k-po-next-translated-entry
      :help "Jump to next translated entry"]
     ["Next fuzzy" k-po-next-fuzzy-entry
      :help "Jump to next fuzzy entry"]
     ["Next obsolete" k-po-next-obsolete-entry
      :help "Jump to next obsolete entry"]
     ["Next untranslated" k-po-next-untranslated-entry
      :help "Jump to next untranslated entry"]
     ["Last file entry" k-po-last-entry
      :help "Jump to last entry"]
     "---"
     ;; Backward
     ["Any previous" k-po-previous-entry
      :help "Jump to previous entry"]
     ["Previous translated" k-po-previous-translated-entry
      :help "Jump to previous translated entry"]
     ["Previous fuzzy" k-po-previous-fuzzy-entry
      :help "Jump to previous fuzzy entry"]
     ["Previous obsolete" k-po-previous-obsolete-entry
      :help "Jump to previous obsolete entry"]
     ["Previous untranslated" k-po-previous-untranslated-entry
      :help "Jump to previous untranslated entry"]
     ["First file entry" k-po-first-entry
      :help "Jump to first entry"]
     "---"
     ;; "Position stack"
     ["Mark and push current" k-po-push-location
      :help "Remember current location"]
     ["Pop and return" k-po-pop-location
      :help "Jump to last remembered location and forget about it"]
     ["Exchange current/top" k-po-exchange-location
      :help "Jump to last remembered location and remember current location"]
     "---"
     ["Redisplay" k-po-display-current-entry
      :help "Make current entry properly visible"]
     ["Current index" k-po-statistics
      :help "Statistical info on current translation file"])
    ("Modifying entries"
     ;; "Msgstr"
     ["Edit msgstr" k-po-edit-msgstr
      :help "Edit current translation"]
     ["Ediff and merge msgstr" k-po-edit-msgstr-and-ediff
      :help "Call `ediff' on current translation for merging"]
     ["Cut msgstr" k-po-kill-msgstr
      :help "Cut (kill) current translation"]
     ["Copy msgstr" k-po-kill-ring-save-msgstr
      :help "Copy current translation"]
     ["Paste msgstr" k-po-yank-msgstr
      :help "Paste (yank) text most recently cut/copied translation"]
     "---"
     ;; "Comments"
     ["Edit comment" k-po-edit-comment
      :help "Edit current comment"]
     ["Ediff and merge comment" k-po-edit-comment-and-ediff
      :help "Call `ediff' on current comment for merging"]
     ["Cut comment" k-po-kill-comment
      :help "Cut (kill) current comment"]
     ["Copy comment" k-po-kill-ring-save-comment
      :help "Copy current translation"]
     ["Paste comment" k-po-yank-comment
      :help "Paste (yank) text most recently cut/copied"]
     "---"
     ["Remove fuzzy mark" k-po-unfuzzy
      :help "Remove \"#, fuzzy\""]
     ["Fuzzy or fade out" k-po-fade-out-entry
      :help "Set current entry fuzzy, or if already fuzzy delete it"]
     ["Init with msgid" k-po-msgid-to-msgstr
      :help "Initialize or replace current translation with the original message"])
    ("Other files"
     ["Other window" k-po-other-window
      :help "Select other window; if necessay split current frame"]
     "---"
     ;; "Program sources"
     ["Cycle reference in source file" k-po-cycle-source-reference t]
     ["Select reference" k-po-select-source-reference t]
     ["Consider path" k-po-consider-source-path t]
     ["Ignore path" k-po-ignore-source-path t]
     ;; "---"
     ;; ;; "Compendiums"
     ;; ["To add entry to compendium" k-po-save-entry nil]
     ;; ["Select from compendium, save" k-po-select-and-save-entry nil]
     "---"
     ;; "---"
     ;; ;; "Lexicography"
     ;; ["Lookup translation" k-po-lookup-lexicons nil]
     ;; ["Add/edit translation" k-po-edit-lexicon-entry nil]
     ;; ["Consider lexicon" k-po-consider-lexicon-file nil]
     ;; ["Ignore lexicon" k-po-ignore-lexicon-file nil])
     "---"
     "Source marking"
     ["Find first string" (k-po-tags-search '(nil)) t]
     ["Prefer keyword" (k-po-select-mark-and-mark '(nil)) t]
     ["Find next string" k-po-tags-search t]
     ["Mark preferred" k-po-mark-translatable t]
     ["Mark with keyword" k-po-select-mark-and-mark t]
     "---"
     ["Validate" k-po-validate
      :help "Check validity of current translation file using `msgfmt'"]
     ["Mail officially" k-po-send-mail
      :help "Send current translation file to the Translation Robot by mail"]
     ["Edit out full" k-po-edit-out-full
      :help "Leave PO mode to edit translation file using fundamental mode"]
     "---"
     ["Forceful quit" k-po-quit
      :help "Close (kill) current translation file without saving"]
     ["Soft quit" k-po-confirm-and-quit
      :help "Save current translation file, than close (kill) it"])))

(defvar k-po-subedit-mode-syntax-table
  (copy-syntax-table text-mode-syntax-table)
  "Syntax table used while in PO mode.")

(defconst k-po-subedit-mode-menu-layout
  `("PO-Edit"
    ["Ediff and merge translation variants" k-po-subedit-ediff
     :help "Call `ediff' for merging variants"]
    "---"
    ["Abort edit" k-po-subedit-abort
     :help "Don't change the translation"]
    ["Exit edit" k-po-subedit-exit
     :help "Use this text as the translation and close current edit buffer"]))

(defconst k-po-subedit-message
  "Type \\[k-po-subedit-exit] once done, or \\[k-po-subedit-abort] to abort edit"
  "Message to post in the minibuffer when an edit buffer is displayed.")

;; Font lock based highlighting code.
(defconst k-po-font-lock-keywords
  '(
    ("^# .*\\|^#[:,]?" . font-lock-comment-face)
    ("^#:\\(.*\\)" 1 font-lock-constant-face)
    ("^#,\\(.*\\)" 1 font-lock-function-name-face)
    ("^\\(\\(msg\\(ctxt\\|id\\(_plural\\)?\\|str\\(\\[[0-9]\\]\\)?\\)\\) \\)?\"\\|\"$"
     . font-lock-keyword-face)
    ("\\\\.\\|%[$-.0-9hjltuzL]*[a-zA-Z]" . font-lock-variable-name-face))
  "Additional expressions to highlight in PO mode.")


;;; Mode activation.

(defvar k-po-mode-abbrev-table nil
  "Abbrev table used while in PO mode.")
(define-abbrev-table 'k-po-mode-abbrev-table ())

(defvar k-po-mode-map
  (let ((k-po-mode-map (make-sparse-keymap)))
    (define-key k-po-mode-map (kbd "C-c C-i")   #'k-po-unfuzzy)
    (define-key k-po-mode-map (kbd "C-c C-j")   #'k-po-msgid-to-msgstr)
    (define-key k-po-mode-map (kbd "C-c C-m")   #'k-po-edit-msgstr)
    (define-key k-po-mode-map (kbd "C-c SPC")   #'k-po-auto-select-entry)
    (define-key k-po-mode-map (kbd "C-c #")     #'k-po-edit-comment)
    (define-key k-po-mode-map (kbd "C-c .")     #'k-po-display-current-entry)
    (define-key k-po-mode-map (kbd "C-c <")     #'k-po-first-entry)
    (define-key k-po-mode-map (kbd "C-c =")     #'k-po-statistics)
    (define-key k-po-mode-map (kbd "C-c >")     #'k-po-last-entry)
    ;; (define-key k-po-mode-map (kbd "C-c c")  #'k-po-save-entry)
    (define-key k-po-mode-map (kbd "C-c f")     #'k-po-next-fuzzy-entry)
    (define-key k-po-mode-map (kbd "C-c k")     #'k-po-kill-msgstr)
    (define-key k-po-mode-map (kbd "C-c l")     #'k-po-sidebar-toggle)
    (define-key k-po-mode-map (kbd "C-c m")     #'k-po-push-location)
    (define-key k-po-mode-map (kbd "C-c n")     #'k-po-next-entry)
    (define-key k-po-mode-map (kbd "C-c o")     #'k-po-next-obsolete-entry)
    (define-key k-po-mode-map (kbd "C-c p")     #'k-po-previous-entry)
    (define-key k-po-mode-map (kbd "C-c q")     #'k-po-confirm-and-quit)
    (define-key k-po-mode-map (kbd "C-c r")     #'k-po-pop-location)
    (define-key k-po-mode-map (kbd "C-c s")     #'k-po-cycle-source-reference)
    (define-key k-po-mode-map (kbd "C-c t")     #'k-po-next-translated-entry)
    (define-key k-po-mode-map (kbd "C-c u")     #'k-po-next-untranslated-entry)
    (define-key k-po-mode-map (kbd "C-c w")     #'k-po-kill-ring-save-msgstr)
    (define-key k-po-mode-map (kbd "C-c x")     #'k-po-exchange-location)
    (define-key k-po-mode-map (kbd "C-c y")     #'k-po-yank-msgstr)
    (define-key k-po-mode-map (kbd "C-c E")     #'k-po-edit-out-full)
    (define-key k-po-mode-map (kbd "C-c F")     #'k-po-previous-fuzzy-entry)
    (define-key k-po-mode-map (kbd "C-c K")     #'k-po-kill-comment)
    ;; (define-key k-po-mode-map (kbd "C-c L")  #'k-po-consider-lexicon-file)
    (define-key k-po-mode-map (kbd "C-c M")     #'k-po-send-mail)
    (define-key k-po-mode-map (kbd "C-c O")     #'k-po-previous-obsolete-entry)
    (define-key k-po-mode-map (kbd "C-c T")     #'k-po-previous-translated-entry)
    (define-key k-po-mode-map (kbd "C-c U")     #'k-po-previous-untranslated-entry)
    (define-key k-po-mode-map (kbd "C-c Q")     #'k-po-quit)
    (define-key k-po-mode-map (kbd "C-c S")     #'k-po-consider-source-path)
    (define-key k-po-mode-map (kbd "C-c V")     #'k-po-validate)
    (define-key k-po-mode-map (kbd "C-c W")     #'k-po-kill-ring-save-comment)
    (define-key k-po-mode-map (kbd "C-c Y")     #'k-po-yank-comment)
    (define-key k-po-mode-map (kbd "C-c 0")     #'k-po-other-window)
    (define-key k-po-mode-map (kbd "C-c DEL")   #'k-po-fade-out-entry)
    (define-key k-po-mode-map (kbd "C-c C-e")   #'k-po-edit-msgstr-and-ediff)
    (define-key k-po-mode-map (kbd "C-c C-#")   #'k-po-edit-comment-and-ediff)
    (define-key k-po-mode-map (kbd "C-c C-C")   #'k-po-edit-comment-and-ediff)
    ;; (define-key k-po-mode-map (kbd "M-c")    #'k-po-select-and-save-entry)
    ;; (define-key k-po-mode-map (kbd "M-l")    #'k-po-edit-lexicon-entry)
    (define-key k-po-mode-map (kbd "M-s")       #'k-po-select-source-reference)
    ;; (define-key k-po-mode-map (kbd "M-L")    #'k-po-ignore-lexicon-file)
    (define-key k-po-mode-map (kbd "M-S")       #'k-po-ignore-source-path)
    k-po-mode-map)
  "Keymap for PO mode.")

;;;###autoload
(define-derived-mode k-po-mode fundamental-mode
  "PO"
  "Major mode for translators to edit PO files.

Special commands:
\\{k-po-mode-map}
Turning on PO mode calls the value of the variable `k-po-mode-hook',
if that value is non-nil.  Behaviour may be adjusted through some variables,
all reachable through \\[customize], in group `Emacs.Editing.I18n.K-po'."
  (when (fboundp 'easy-menu-define)
    (easy-menu-define k-po-mode-menu k-po-mode-map "" k-po-mode-menu-layout))
  (setq-local font-lock-defaults '(k-po-font-lock-keywords t))

  (setq-local k-po-mode-flag t)

  (k-po-check-file-header)
  (k-po-compute-counters nil)
  (when k-po-insert-memory
    (k-po-memory-insert-current-file))

  (setq-local k-po-edited-fields nil)
  (setq-local k-po-marker-stack nil)
  (setq-local k-po-search-path '(("./") ("../")))

  (setq-local k-po-reference-alist nil)
  (setq-local k-po-reference-cursor nil)
  (setq-local k-po-reference-check 0)

  (setq-local k-po-keywords '(("gettext") ("gettext_noop") ("_") ("N_")))
  (setq-local k-po-string-contents nil)
  (setq-local k-po-string-buffer nil)
  (setq-local k-po-string-start nil)
  (setq-local k-po-string-end nil)
  (setq-local k-po-marking-overlay (k-po-create-overlay))

  (add-hook 'before-save-hook #'k-po-memory-insert-current-file nil t)
  (add-hook 'post-command-hook #'k-po-sidebar--post-command-h nil t)
  (add-hook 'write-contents-functions #'k-po-replace-revision-date))

(defvar k-po-subedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'k-po-subedit-exit)
    (define-key map (kbd "C-c C-'") #'k-po-subedit-exit)
    (define-key map (kbd "C-c C-e") #'k-po-subedit-ediff)
    (define-key map (kbd "C-c C-k") #'k-po-subedit-abort)
    map)
  "Keymap while editing a PO mode entry (or the full PO file).")


;;; Window management.

(defvar-local k-po-mode-flag nil
  "Non-nil means that MODE-LINE-STRING should be displayed.")

(defvar k-po-mode-line-entry '(k-po-mode-flag ("  " k-po-mode-line-string))
  "Mode line format entry displaying MODE-LINE-STRING.")

;; Insert MODE-LINE-ENTRY in mode line, but on first load only.
(unless (member k-po-mode-line-entry mode-line-format)
  ;; mode-line-format usually contains global-mode-string, but some
  ;; people customize this variable. As a last resort, append at the end.
  (let ((prev-entry (or (member 'global-mode-string mode-line-format)
                        (member "   " mode-line-format)
                        (last mode-line-format))))
    (setcdr prev-entry (cons k-po-mode-line-entry (cdr prev-entry)))))

(defun k-po-update-mode-line-string ()
  "Compute a new statistics string to display in mode line."
  (setq k-po-mode-line-string
        (concat (format "%dt" k-po-translated-counter)
                (if (> k-po-fuzzy-counter 0)
                    (format "+%df" k-po-fuzzy-counter))
                (if (> k-po-untranslated-counter 0)
                    (format "+%du" k-po-untranslated-counter))
                (if (> k-po-obsolete-counter 0)
                    (format "+%do" k-po-obsolete-counter))))
  (force-mode-line-update))

(defun k-po-type-counter ()
  "Return the symbol name of the counter appropriate for the current entry."
  (cond ((eq k-po-entry-type 'obsolete) 'k-po-obsolete-counter)
        ((eq k-po-entry-type 'fuzzy) 'k-po-fuzzy-counter)
        ((eq k-po-entry-type 'translated) 'k-po-translated-counter)
        ((eq k-po-entry-type 'untranslated) 'k-po-untranslated-counter)
        (t (error "Unknown entry type"))))

(defun k-po-decrease-type-counter ()
  "Decrease the counter corresponding to the nature of the current entry."
  (let ((counter (k-po-type-counter)))
    (cl-decf (symbol-value counter))))

(defun k-po-increase-type-counter ()
  "Increase the counter corresponding to the nature of the current entry.
Then, update the mode line counters."
  (let ((counter (k-po-type-counter)))
    (cl-incf (symbol-value counter)))
  (k-po-update-mode-line-string))

(defun k-po-map-entries (func &optional reporter)
  "Call FUNC for each entry in the buffer.
FUNC is called with the entry as its only argument.
If REPORTER is non-nil, it is ticked using
`progress-reporter-update' with the point position after each
iteration, and finalized using `progress-reporter-done' afterwards."
  (save-excursion
    (goto-char (point-min))
    (funcall func (k-po-current-entry))
    (when (re-search-forward "^msgid" nil t)
      (while (and (re-search-forward k-po-any-msgstr-block-regexp nil t)
                  (not (eobp)))
        (funcall func (k-po-current-entry))
        (when reporter
          (progress-reporter-update reporter (point))))
      (when reporter
        (progress-reporter-done reporter)))))

(defun k-po-compute-counters (echo)
  "Prepare counters for mode line display.  If ECHO, also echo entry position."
  (setq k-po-translated-counter 0
        k-po-fuzzy-counter 0
        k-po-untranslated-counter 0
        k-po-obsolete-counter 0)
  (let ((position 0) (total 0) current here)
    (save-excursion
      (setq current (k-po-entry-msgstr-block-start (k-po-current-entry)))
      (goto-char (point-min))
      ;; While counting, skip the header entry, for consistency with msgfmt.
      (let ((entry (k-po-current-entry)))
        (when (string-equal (k-po-entry-msgid entry) "")
          (goto-char (k-po-entry-end entry))))
      (when (re-search-forward "^msgid" (point-max) t)
        ;; Start counting
        (while (re-search-forward k-po-any-msgstr-block-regexp nil t)
          (and (= (% total 20) 0)
               (if echo
                   (message "Position %d/%d" position total)
                 (message "Position %d" total)))
          (setq here (point))
          (goto-char (match-beginning 0))
          (setq total (1+ total))
          (and echo (eq (point) current) (setq position total))
          (cond ((eq (following-char) ?#)
                 (setq k-po-obsolete-counter (1+ k-po-obsolete-counter)))
                ((looking-at k-po-untranslated-regexp)
                 (setq k-po-untranslated-counter (1+ k-po-untranslated-counter)))
                (t (setq k-po-translated-counter (1+ k-po-translated-counter))))
          (goto-char here))

        ;; Make another pass just for the fuzzy entries, kind of kludgey.
        ;; FIXME: Counts will be wrong if untranslated entries are fuzzy, yet
        ;; this should not normally happen.
        (goto-char (point-min))
        (while (re-search-forward k-po-fuzzy-regexp nil t)
          (setq k-po-fuzzy-counter (1+ k-po-fuzzy-counter)))
        (setq k-po-translated-counter (- k-po-translated-counter k-po-fuzzy-counter))))

    ;; Push the results out.
    (if echo
        (message "\
Position %d/%d; %d translated, %d fuzzy, %d untranslated, %d obsolete"
                 position total k-po-translated-counter k-po-fuzzy-counter
                 k-po-untranslated-counter k-po-obsolete-counter)
      (message "")))
  (k-po-update-mode-line-string))

(defun k-po-redisplay (&optional entry)
  "Redisplay ENTRY or the current entry."
  ;; TODO: Should try to fit the whole entry on the window.  If this is not
  ;; possible, should try to fit the comment and the msgid.  Otherwise,
  ;; should try to fit the msgid.  Else, the first line of the msgid should
  ;; be at the top of the window.
  (goto-char (k-po-entry-msgid-start (or entry (k-po-current-entry)))))

(defun k-po-other-window ()
  "Get the cursor into another window, out of PO mode."
  (interactive)
  (if (one-window-p t)
      (progn
        (split-window)
        (switch-to-buffer (other-buffer)))
    (other-window 1)))

;;; Processing the PO file header entry.

(defun k-po-check-file-header ()
  "Create a missing PO mode file header, or replace an oldish one.
Can be customized with the `k-po-auto-update-file-header' variable."
  ;; FIXME: update file header on SAVE not OPEN.
  (if (or (eq k-po-auto-update-file-header t)
          (and (eq k-po-auto-update-file-header 'ask)
               (y-or-n-p "May I update the PO Header Entry? ")))
      (save-excursion
        (save-restriction
          (widen) ; in case of a narrowed view to the buffer
          (let (insert-flag)
            (goto-char (point-min))
            (let ((header (k-po-goto-header)))
              (when (eq 'legacy (car header))
                ;; This is an oldish header.  Replace it all.
                (setq insert-flag t)
                (goto-char (cdr header))
                (while (> (point) (point-min))
                  (forward-line -1)
                  (insert "#~ ")
                  (beginning-of-line))
                (beginning-of-line))
              (unless header
                (setq insert-flag t)))
            (goto-char (point-min))
            (when insert-flag
              (insert k-po-default-file-header)
              (if (not (eobp))
                  (insert "\n"))))))))

(defun k-po-replace-revision-date ()
  "Replace the revision date by current time in the PO file header."
  (if (fboundp 'format-time-string)
      (if (or (eq k-po-auto-replace-revision-date t)
              (and (eq k-po-auto-replace-revision-date 'ask)
                   (y-or-n-p "May I set PO-Revision-Date? ")))
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "^\"PO-Revision-Date:.*" nil t)
                (let* ((time (current-time))
                       (seconds (or (car (current-time-zone time)) 0))
                       (minutes (/ (abs seconds) 60))
                       (zone (format "%c%02d%02d"
                                     (if (< seconds 0) ?- ?+)
                                     (/ minutes 60)
                                     (% minutes 60))))
                  (replace-match
                   (concat "\"PO-Revision-Date: "
                           (format-time-string "%Y-%m-%d %H:%M" time)
                           zone "\\n\"")
                   t t))))
        (message ""))
    (message "PO-Revision-Date should be adjusted..."))
  ;; Return nil to indicate that the buffer has not yet been saved.
  nil)

;;; "Current-*" functions

(defun k-po--language->code (language)
  "Return the language code for LANGUAGE."
  ;; This is not technically pure, since it reads from `k-po-team-name-to-code'.
  ;; But that variable is meant to be a constant, and can be seen as just a
  ;; (leaky) implementation detail, so I'm still going to mark this as pure to
  ;; stand out from a sea of ancient functions that are swimming in global
  ;; variables.
  (declare (pure t) (side-effect-free t))
  (let ((pair (or (assoc language k-po-team-name-to-code)
                  ;; For if the value is the code itself
                  (rassoc language k-po-team-name-to-code))))
    (cdr pair)))

(defun k-po--language<-code (code)
  "Return the name for the language identified by CODE."
  ;; This is not technically pure, since it reads from `k-po-team-name-to-code'.
  ;; But that variable is meant to be a constant, and can be seen as just a
  ;; (leaky) implementation detail, so I'm still going to mark this as pure to
  ;; stand out from a sea of ancient functions that are swimming in global
  ;; variables.
  (declare (pure t) (side-effect-free t))
  (let ((pair (or (assoc code k-po-team-name-to-code)
                  ;; For if the value is the code itself
                  (rassoc code k-po-team-name-to-code))))
    (car pair)))

(defun k-po-current-source-language ()
  "Return the source language code of the current file.
This currently always returns English (\"en\")."
  (declare (side-effect-free t))
  "en")

(defun k-po-current-target-language ()
  "Return the target language code of the current file."
  ;; I'm going to mark this as without side effects since that's how this is
  ;; intended to be used.
  (declare (side-effect-free t))
  (save-match-data
    (save-excursion
      (let ((header (k-po-goto-header)))
        ;; If we don't find a header, blow up
        (unless header
          (error "No header found in current file!"))
        ;; We *should* find a language field. Let re-search-forward error.
        (re-search-forward (rx bol "\""
                               (opt (group "X-")) ; some files have "X-Language"
                               "Language:" (one-or-more " ")
                               (group (zero-or-more nonl))
                               ;; A backslash and an in the po file
                               "\\n\"" eol)
                           (cdr header))
        (k-po--language->code (match-string 2))))))


;;; Handling span of entry, entry type and entry attributes.

(defun k-po-mode-add-attribute (entry name)
  "Add attribute NAME to ENTRY, unless it is already there."
  (save-excursion
    (goto-char (oref entry start))
    (if (re-search-forward "\n#, .*" (oref entry msgctxt-start) t)
        (save-restriction
          (narrow-to-region (match-beginning 0) (match-end 0))
          (goto-char (point-min))
          (if (re-search-forward (concat "\\b" name "\\b") nil t)
              nil
            (goto-char (point-max))
            (insert ", " name)))
      (skip-chars-forward "\n")
      (while (eq (following-char) ?#)
        (forward-line 1))
      (insert "#, " name "\n"))))

(defun k-po-mode-delete-attribute (entry name)
  "Delete attribute NAME from ENTRY, if any."
  (save-excursion
    (goto-char (oref entry start))
    (when (re-search-forward "\n#, .*" (oref entry msgctxt-start) t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (goto-char (point-min))
        (if (re-search-forward
             (format "\\(\n#, %s$\\|, %s$\\| %s,\\)"
                     name name name)
             nil t)
            (replace-match "" t t))))))

;;; Entry positionning.

(defun k-po-say-location-depth ()
  "Tell how many entries in the entry location stack."
  (let ((depth (length k-po-marker-stack)))
    (cond ((= depth 0) (message "Empty location stack"))
          ((= depth 1) (message "One entry in location stack"))
          (t (message "%d entries in location stack" depth)))))

(defun k-po-push-location ()
  "Stack the location of the current entry, for later return."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (save-excursion
      (goto-char (k-po-entry-msgid-start entry))
      (setq k-po-marker-stack (cons (point-marker) k-po-marker-stack)))
    (k-po-say-location-depth)))

(defun k-po-pop-location ()
  "Unstack a saved location, and return to the corresponding entry."
  (interactive)
  (if k-po-marker-stack
      (progn
        (goto-char (car k-po-marker-stack))
        (setq k-po-marker-stack (cdr k-po-marker-stack))
        (k-po-display-current-entry)
        (k-po-say-location-depth))
    (error "The entry location stack is empty")))

(defun k-po-exchange-location ()
  "Exchange the location of the current entry with the top of stack."
  (interactive)
  (unless k-po-marker-stack
    (error "The entry location stack is empty"))
  (let ((entry (k-po-current-entry)))
    (goto-char (k-po-entry-msgid-start entry))
    (let ((location (point-marker)))
      (goto-char (car k-po-marker-stack))
      (setq k-po-marker-stack (cons location (cdr k-po-marker-stack))))
    (k-po-display-current-entry)
    (k-po-say-location-depth)))

(defun k-po-display-current-entry ()
  "Display the current entry."
  (interactive)
  (k-po-redisplay
   (k-po-current-entry)))

(defun k-po-first-entry-with-regexp (regexp)
  "Display the first entry in the file which msgstr matches REGEXP."
  (let ((here (point)))
    (goto-char (point-min))
    (if (re-search-forward regexp nil t)
        (progn
          (goto-char (match-beginning 0))
          (k-po-display-current-entry))
      (goto-char here)
      (error "There is no such entry"))))

(defun k-po-last-entry-with-regexp (regexp)
  "Display the last entry in the file which msgstr matches REGEXP."
  (let ((here (point)))
    (goto-char (point-max))
    (if (re-search-backward regexp nil t)
        (k-po-display-current-entry)
      (goto-char here)
      (error "There is no such entry"))))

(defun k-po-next-entry-with-regexp (regexp wrap)
  "Display the entry following the current entry which msgstr matches REGEXP.
If WRAP is not nil, the search may wrap around the buffer."
  (let ((entry (k-po-current-entry))
        (here (point)))
    (goto-char (oref entry end))
    (if (re-search-forward regexp nil t)
        (progn
          (goto-char (match-beginning 0))
          (k-po-display-current-entry))
      (if (and wrap
               (progn
                 (goto-char (point-min))
                 (re-search-forward regexp (oref entry start) t)))
          (progn
            (goto-char (match-beginning 0))
            (k-po-display-current-entry)
            (message "Wrapping around the buffer"))
        (goto-char here)
        (error "There is no such entry")))))

(defun k-po-previous-entry-with-regexp (regexp wrap)
  "Redisplay the entry preceding the current entry which msgstr matches REGEXP.
If WRAP is not nil, the search may wrap around the buffer."
  (let ((entry (k-po-current-entry))
        (here (point)))
    (goto-char (oref entry start))
    (if (re-search-backward regexp nil t)
        (k-po-display-current-entry)
      (if (and wrap
               (progn
                 (goto-char (point-max))
                 (re-search-backward regexp (oref entry end) t)))
          (progn
            (k-po-display-current-entry)
            (message "Wrapping around the buffer"))
        (goto-char here)
        (error "There is no such entry")))))

;; Any entries.

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

(defun k-po-jump-to-entry (source target)
  "Jump to the entry whose msgid is SOURCE and msgstr is TARGET."
  (let ((found
         (catch 'found
           (k-po-map-entries
            (lambda (entry)
              (when (and (equal source (k-po-entry-msgid entry))
                         (equal target (k-po-entry-msgstr entry)))
                (throw 'found (point)))))
           ;; If we reach this point, we haven't found it.
           nil)))
    (when found (goto-char found))))

(defun k-po-first-entry ()
  "Display the first entry."
  (interactive)
  (k-po-first-entry-with-regexp k-po-any-msgstr-block-regexp))

(defun k-po-last-entry ()
  "Display the last entry."
  (interactive)
  (k-po-last-entry-with-regexp k-po-any-msgstr-block-regexp))

(defun k-po-next-entry ()
  "Display the entry following the current entry."
  (interactive)
  (k-po-next-entry-with-regexp k-po-any-msgstr-block-regexp nil))

(defun k-po-previous-entry ()
  "Display the entry preceding the current entry."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-any-msgstr-block-regexp nil))

;; Untranslated entries.

(defun k-po-next-untranslated-entry ()
  "Find the next untranslated entry, wrapping around if necessary."
  (interactive)
  (k-po-next-entry-with-regexp k-po-untranslated-regexp t))

(defun k-po-previous-untranslated-entry ()
  "Find the previous untranslated entry, wrapping around if necessary."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-untranslated-regexp t))

(defun k-po-msgid-to-msgstr ()
  "Use another window to edit msgstr reinitialized with msgid."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (if (or (k-po-entry-type? entry 'untranslated)
            (k-po-entry-type? entry 'obsolete)
            (prog1 (y-or-n-p "Really lose previous translation? ")
              (message "")))
        ;; In an entry with plural forms, use the msgid_plural string,
        ;; as it is more general than the msgid string.
        (if (k-po-set-msgstr-form (or (k-po-entry-msgid_plural entry)
                                      (k-po-entry-msgid entry))
                                  entry)
            (k-po-maybe-delete-previous-untranslated)))))

;; Obsolete entries.

(defun k-po-next-obsolete-entry ()
  "Find the next obsolete entry, wrapping around if necessary."
  (interactive)
  (k-po-next-entry-with-regexp k-po-obsolete-msgstr-regexp t))

(defun k-po-previous-obsolete-entry ()
  "Find the previous obsolete entry, wrapping around if necessary."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-obsolete-msgstr-regexp t))

;; Fuzzy entries.

(defun k-po-next-fuzzy-entry ()
  "Find the next fuzzy entry, wrapping around if necessary."
  (interactive)
  (k-po-next-entry-with-regexp k-po-fuzzy-regexp t))

(defun k-po-previous-fuzzy-entry ()
  "Find the next fuzzy entry, wrapping around if necessary."
  (interactive)
  (k-po-previous-entry-with-regexp k-po-fuzzy-regexp t))

(defun k-po-toggle-fuzzy ()
  "Toggle fuzzy for the current entry."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (if (k-po-entry-type? entry 'fuzzy)
        (k-po-mode-delete-attribute entry "fuzzy")
      (k-po-mode-add-attribute entry "fuzzy"))))

(defun k-po--unfuzzy (entry)
  "Unfuzzy ENTRY then update other state."
  (when (k-po-entry-type? entry 'fuzzy)
    (k-po-decrease-type-counter)
    (k-po-mode-delete-attribute entry "fuzzy")
    (k-po-increase-type-counter)))

(defun k-po-unfuzzy-all ()
  "Unfuzzy all entries in the current file."
  (interactive)
  (goto-char (point-min))
  (k-po-map-entries
   (lambda (entry)
     (catch 'continue
       (unless (k-po-entry-type? entry 'fuzzy)
         (throw 'continue t))
       (k-po--unfuzzy entry)))
   (make-progress-reporter "Unfuzzying entries..." 1 (point-max))))

(defun k-po-unfuzzy ()
  "Remove the fuzzy attribute for the current entry."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (when (k-po-entry-type? entry 'fuzzy)
      (k-po-decrease-type-counter)
      (k-po-mode-delete-attribute entry "fuzzy")
      (k-po-maybe-delete-previous-untranslated)
      (k-po-display-current-entry)
      (k-po-increase-type-counter))
    (when k-po-auto-select-on-unfuzzy
      (k-po-auto-select-entry))
    (k-po-update-mode-line-string)))

;; Translated entries.

(defun k-po-next-translated-entry ()
  "Find the next translated entry, wrapping around if necessary."
  (interactive)
  (if (= k-po-translated-counter 0)
      (error "There is no such entry")
    (k-po-next-entry-with-regexp k-po-any-msgstr-block-regexp t)
    (let ((entry (k-po-current-entry)))
      (while (not (k-po-entry-type? entry 'translated))
        (k-po-next-entry-with-regexp k-po-any-msgstr-block-regexp t)
        (setq entry (k-po-current-entry))))))

(defun k-po-previous-translated-entry ()
  "Find the previous translated entry, wrapping around if necessary."
  (interactive)
  (if (= k-po-translated-counter 0)
      (error "There is no such entry")
    (k-po-previous-entry-with-regexp k-po-any-msgstr-block-regexp t)
    (let ((entry (k-po-current-entry)))
      (while (not (k-po-entry-type? entry 'translated))
        (k-po-previous-entry-with-regexp k-po-any-msgstr-block-regexp t)
        (setq entry (k-po-current-entry))))))

;; Auto-selection feature.

(defun k-po-auto-select-entry ()
  "Select the next entry having the same type as the current one.
If none, wrap from the beginning of the buffer with another type,
going from untranslated to fuzzy, and from fuzzy to obsolete.
Plain translated entries are always disregarded unless there are
no entries of the other types."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (goto-char (oref entry end))
    (if (and (= k-po-untranslated-counter 0)
             (= k-po-fuzzy-counter 0)
             (= k-po-obsolete-counter 0))
        ;; All entries are plain translated.  Next entry will do, or
        ;; wrap around if there is none.
        (if (re-search-forward k-po-any-msgstr-block-regexp nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-min)))
      ;; If over a translated entry, look for an untranslated one first.
      ;; Else, look for an entry of the same type first.
      (let ((goal (if (k-po-entry-type? entry 'translated)
                      'untranslated
                    (oref entry type))))
        (while goal
          ;; Find an untranslated entry, or wrap up for a fuzzy entry.
          (if (eq goal 'untranslated)
              (if (and (> k-po-untranslated-counter 0)
                       (re-search-forward k-po-untranslated-regexp nil t))
                  (progn
                    (goto-char (match-beginning 0))
                    (setq goal nil))
                (goto-char (point-min))
                (setq goal 'fuzzy)))
          ;; Find a fuzzy entry, or wrap up for an obsolete entry.
          (if (eq goal 'fuzzy)
              (if (and (> k-po-fuzzy-counter 0)
                       (re-search-forward k-po-fuzzy-regexp nil t))
                  (progn
                    (goto-char (match-beginning 0))
                    (setq goal nil))
                (goto-char (point-min))
                (setq goal 'obsolete)))
          ;; Find an obsolete entry, or wrap up for an untranslated entry.
          (if (eq goal 'obsolete)
              (if (and (> k-po-obsolete-counter 0)
                       (re-search-forward k-po-obsolete-msgstr-regexp nil t))
                  (progn
                    (goto-char (match-beginning 0))
                    (setq goal nil))
                (goto-char (point-min))
                (setq goal 'untranslated)))))))
  ;; Display this entry nicely.
  (k-po-display-current-entry))

;;; Killing and yanking fields.

(defun k-po-set-msgid (func &optional entry)
  "Set the msgid of the entry at point, using FUNC to get a string.
Calling FUNC should insert the wanted string in the current
buffer. If FUNC is itself a string, then this string is used for
insertion. The string is properly requoted before the replacement
occurs.

If ENTRY is non-nil, use that instead of getting the entry at
point again.

Return nil if the buffer has not been modified, for if the new msgid
described by CALL is merely identical to the msgid already in place."
  (unless entry
    (setq entry (k-po-current-entry)))
  (let ((string (k-po-call-requoted func "msgid" (k-po-entry-type? entry 'obsolete))))
    (save-excursion
      (goto-char (oref entry start))
      (re-search-forward k-po-any-msgid-regexp (oref entry msgstr-block-start))
      (and (not (string-equal (match-string-no-properties 0) string))
           (replace-match string t t)
           (goto-char (oref entry msgid-start))
           (k-po-find-span-of-entry)
           t))))

(defun k-po-set-msgstr-form (func &optional entry)
  "Replace the msgstr or msgstr[] of the current entry, using FUNC to get a string.
Calling FUNC should insert the wanted string in the current
buffer. If FUNC is itself a string, then this string is used for
insertion. The string is properly requoted before the replacement
occurs.

If ENTRY is non-nil, use that as the current entry instead of
figuring it out from the buffer again.

Return nil if the buffer has not been modified, for if the new msgstr
described by FORM is merely identical to the msgstr already in place."
  (unless entry
    (setq entry (k-po-current-entry)))
  (let ((string (k-po-call-requoted func
                                    (k-po-entry-msgstr-flavor entry)
                                    (k-po-entry-type? entry 'obsolete))))
    (save-excursion
      (goto-char (k-po-entry-msgstr-form-start entry))
      (re-search-forward k-po-any-msgstr-form-regexp (k-po-entry-msgstr-form-end entry))
      (and (not (string-equal (match-string-no-properties 0) string))
           (k-po-decrease-type-counter)
           (replace-match string t t)
           (goto-char (k-po-entry-msgid-start entry))
           (k-po-find-span-of-entry)
           (k-po-increase-type-counter)
           t))))

(defun k-po-kill-ring-save-msgstr (&optional entry)
  "Push the msgstr string from ENTRY on the kill ring.
ENTRY defaults to the current entry."
  (interactive)
  (let ((string (k-po-entry-msgstr (or entry (k-po-current-entry)))))
    (kill-new string)
    string))

(defun k-po-kill-msgstr ()
  "Empty the msgstr string from current entry, pushing it on the kill ring."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (k-po-kill-ring-save-msgstr entry)
    (when (k-po-set-msgstr-form "" entry)
      (k-po-maybe-delete-previous-untranslated))))

(defun k-po-yank-msgstr ()
  "Replace the current msgstr string by the top of the kill ring."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (when (k-po-set-msgstr-form
           (lambda ()
             (if (eq last-command 'yank)
                 (yank-pop 1)
               (yank)))
           entry)
      (k-po-maybe-delete-previous-untranslated))
    (setq this-command 'yank)))

(defun k-po-fade-out-entry ()
  "Mark an active entry as fuzzy; obsolete a fuzzy or untranslated entry;
or completely delete an obsolete entry, saving its msgstr on the kill ring."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (cond ((k-po-entry-type? entry 'translated)
           (k-po-decrease-type-counter)
           (k-po-mode-add-attribute entry "fuzzy")
           (k-po-display-current-entry)
           (k-po-increase-type-counter))

          ((or (k-po-entry-type? entry 'fuzzy)
               (k-po-entry-type? entry 'untranslated))
           (when (y-or-n-p "Should I really obsolete this entry? ")
             (k-po-decrease-type-counter)
             (save-excursion
               (save-restriction
                 (narrow-to-region (oref entry start)
                                   (oref entry end))
                 (goto-char (point-min))
                 (skip-chars-forward "\n")
                 (while (not (eobp))
                   (insert "#~ ")
                   (search-forward "\n"))))
             (k-po-display-current-entry)
             (k-po-increase-type-counter))
           (message ""))

          ((and (k-po-entry-type? entry 'obsolete)
                (k-po-check-for-pending-edit (k-po-entry-msgid-start entry))
                (k-po-check-for-pending-edit (k-po-entry-msgstr-block-start entry)))
           (k-po-decrease-type-counter)
           (k-po-update-mode-line-string)
           ;; TODO: Should save all msgstr forms here, not just one.
           (kill-new (k-po-entry-msgstr entry))
           (delete-region (k-po-entry-start entry)
                          (k-po-entry-end entry))
           (goto-char (k-po-entry-start entry))
           (if (re-search-forward k-po-any-msgstr-block-regexp nil t)
               (goto-char (match-beginning 0))
             (re-search-backward k-po-any-msgstr-block-regexp nil t))
           (k-po-display-current-entry)
           (message "")))))

;;; Killing and yanking comments.

(defun k-po-set-comment (str-or-func &optional entry)
  "Use STR-OR-FUNC get a string to replace the current editable comment.

If STR-OR-FUNC is a string, use that. Otherwise, if it is a
function, it should insert the wanted string in the current
buffer.

The string is properly recommented before the replacement occurs.

If ENTRY is non-nil, assume that is the current entry."
  (unless entry
    (setq entry (k-po-current-entry)))
  (let ((obsolete (k-po-entry-type? entry 'obsolete))
        string)
    (with-temp-buffer
      (if (stringp str-or-func)
          (insert str-or-func)
        (push-mark)
        (funcall str-or-func))
      (if (not (or (bobp) (= (preceding-char) ?\n)))
          (insert "\n"))
      (goto-char (point-min))
      (while (not (eobp))
        (insert (if (= (following-char) ?\n) "#" "# "))
        (search-forward "\n"))
      (setq string (buffer-string)))
    (goto-char (oref entry start))
    (if (re-search-forward k-po-comment-regexp (oref entry end) t)
        (if (not (string-equal (match-string-no-properties 0) string))
            (replace-match string t t))
      (skip-chars-forward " \t\n")
      (insert string)))
  (k-po-display-current-entry))

(defun k-po-kill-ring-save-comment (&optional entry)
  "Push the msgstr string from current entry on the kill ring.
If ENTRY is non-nil, use that instead of the current entry."
  (interactive)
  (kill-new
   (k-po-entry-comment
    (or entry (k-po-current-entry)))))

(defun k-po-visit-kde-invent ()
  "Visit source on KDE Invent.
Assumes this is applicable to the current entry."
  (interactive)
  (save-excursion
    (let ((entry (k-po-current-entry)))
      (goto-char (k-po-entry-start entry))
      (when (re-search-forward "\\(^#:\\)? *\\([^: ]*\\):\\([0-9]+\\)"
                               (k-po-entry-msgid-start entry) t)
        (let ((project (file-name-base
                        (directory-file-name
                         (file-name-directory
                          (buffer-file-name)))))
              (name (match-string 2))
              (line (match-string 3)))
          (when (y-or-n-p (format "Project: %s, %s:%s. Proceed?"
                                  project name line))
            (browse-url (format "https://kde-project.kisaragi-hiu.com/%s/%s#L%s"
                                project
                                name
                                line))))))))

(defun k-po-kill-comment ()
  "Empty the msgstr string from current entry, pushing it on the kill ring."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (k-po-kill-ring-save-comment entry)
    (k-po-set-comment "" entry)
    (k-po-redisplay entry)))

(defun k-po-yank-comment ()
  "Replace the current comment string by the top of the kill ring."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (k-po-set-comment
     (lambda ()
       (if (eq last-command 'yank)
           (yank-pop 1)
         (yank)))
     entry)
    (setq this-command 'yank)
    (k-po-redisplay entry)))

;;; Deleting the "previous untranslated" comment.

(defun k-po-previous-untranslated-regions (entry)
  "Return the list of previous untranslated regions in ENTRY."
  (let (ret)
    (dolist (regexp (list k-po-any-previous-msgctxt-regexp
                          k-po-any-previous-msgid-regexp
                          k-po-any-previous-msgid_plural-regexp))
      (save-excursion
        (goto-char (k-po-entry-start entry))
        (when (re-search-forward regexp (k-po-entry-msgctxt-start entry) t)
          (push (cons (copy-marker (match-beginning 0))
                      (copy-marker (match-end 0)))
                ret))))
    (nreverse ret)))

(defun k-po-delete-previous-untranslated ()
  "Delete the previous untranslated fields from the current entry.
The fields are msgctxt, msgid, and msgid_plural (marked as #| comments)."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (dolist (region (k-po-previous-untranslated-regions entry))
      (delete-region (car region) (cdr region)))
    (k-po-redisplay)))

(defun k-po-maybe-delete-previous-untranslated ()
  "Ask to delete the previous untranslated fields from the current entry.
The fields are msgctxt, msgid, and msgid_plural (marked as #| comments)."
  (let* ((entry (k-po-current-entry))
         (previous-regions (k-po-previous-untranslated-regions entry)))
    (when (and previous-regions
               (or (eq k-po-auto-delete-previous-msgid t)
                   (and (eq k-po-auto-delete-previous-msgid 'ask)
                        (let ((overlays nil))
                          (unwind-protect
                              (progn
                                (setq overlays
                                      (mapcar (lambda (region)
                                                (let ((overlay (k-po-create-overlay)))
                                                  (k-po-highlight overlay (car region) (cdr region))
                                                  overlay))
                                              previous-regions))
                                ;; Scroll, to show the previous-regions.
                                (goto-char (car (car previous-regions)))
                                (prog1 (y-or-n-p "Delete previous msgid comments? ")
                                  (message "")))
                            (mapc 'k-po-dehighlight overlays))))))
      (dolist (region previous-regions)
        (delete-region (car region) (cdr region))))))

;;; Editing management and submode.

(defvar k-po-subedit-back-pointer nil
  "Points to one of the slots of the po buffer.

In a string edit buffer, BACK-POINTER points to one of the slots of the
list EDITED-FIELDS kept in the PO buffer. See its description elsewhere.
Reminder: slots have the form (ENTRY-MARKER EDIT-BUFFER OVERLAY-INFO).")

(defun k-po-clean-out-killed-edits ()
  "From EDITED-FIELDS, clean out any edit having a killed edit buffer."
  (let ((cursor k-po-edited-fields))
    (while cursor
      (let ((slot (car cursor)))
        (setq cursor (cdr cursor))
        (if (buffer-name (nth 1 slot))
            nil
          (let ((overlay (nth 2 slot)))
            (and overlay (k-po-dehighlight overlay)))
          (setq k-po-edited-fields (delete slot k-po-edited-fields)))))))

(defun k-po-check-all-pending-edits ()
  "Resume any pending edit.  Return nil if some remains."
  (k-po-clean-out-killed-edits)
  (or (null k-po-edited-fields)
      (let ((slot (car k-po-edited-fields)))
        (goto-char (nth 0 slot))
        (pop-to-buffer (nth 1 slot))
        (message "%s" (substitute-command-keys k-po-subedit-message))
        nil)))

(defun k-po-check-for-pending-edit (position)
  "Resume any pending edit at POSITION.  Return nil if such edit exists."
  (k-po-clean-out-killed-edits)
  (let ((marker (make-marker)))
    (set-marker marker position)
    (let ((slot (assoc marker k-po-edited-fields)))
      (when slot
        (goto-char marker)
        (pop-to-buffer (nth 1 slot))
        (message "%s" (substitute-command-keys k-po-subedit-message)))
      (not slot))))

(defun k-po-edit-out-full ()
  "Get out of PO mode, leaving PO file buffer in fundamental mode."
  (interactive)
  (when (k-po-check-all-pending-edits)
    ;; No need to ask as the user has just explicitly asked for it.
    (fundamental-mode)
    (message (substitute-command-keys "Type \\`M-x k-po-mode RET' once done"))))

(defun k-po-ediff-quit ()
  "Quit ediff and exit `recursive-edit'."
  (interactive)
  (ediff-quit t)
  (exit-recursive-edit))

(add-hook 'ediff-keymap-setup-hook
          #'(lambda ()
              (define-key ediff-mode-map "Q" 'k-po-ediff-quit)))

;; Avoid byte compiler warnings.
(defvar k-po-subedit--entry-buffer)
(defvar-local k-po-subedit--reused? nil
  "Used to mark whether a subedit buffer is in a reused window.")

(defun k-po-ediff-buffers-exit-recursive (b1 b2 oldbuf end)
  "Ediff buffer B1 and B2, pop back to OLDBUF and replace the old variants.
This function will delete the first two variants in OLDBUF, call
`ediff-buffers' to compare both strings and replace the two variants in
OLDBUF with the contents of B2.
Once done kill B1 and B2.

For more info cf. `k-po-subedit-ediff'."
  (ediff-buffers b1 b2)
  (recursive-edit)
  (pop-to-buffer oldbuf)
  (delete-region (point-min) end)
  (insert-buffer-substring b2)
  (mapc 'kill-buffer `(,b1 ,b2))
  (display-buffer k-po-subedit--entry-buffer t))

(defun k-po-subedit-ediff ()
  "Edit the subedit buffer using `ediff'.
`k-po-subedit-ediff' calls `k-po-ediff-buffers-exit-recursive' to
edit translation variants side by side if they are actually
different; if variants are equal just delete the first one.

`msgcat' is able to produce those variants; every variant is marked with:

#-#-#-#-#  file name reference  #-#-#-#-#

Put changes in second buffer.

When done with the `ediff' session press \\[exit-recursive-edit]
exit to `recursive-edit', or call \\[k-po-ediff-quit] (`Q') in
the ediff control panel."
  (interactive)
  (let* ((marker-regex "^#-#-#-#-#  \\(.*\\)  #-#-#-#-#\n")
         (buf1 " *k-po-msgstr-1") ; default if first marker is missing
         buf2 start-1 end-1 start-2 end-2
         (back-pointer k-po-subedit-back-pointer)
         (entry-marker (nth 0 back-pointer))
         (k-po-subedit--entry-buffer (marker-buffer entry-marker)))
    (goto-char (point-min))
    (if (looking-at marker-regex)
        (and (setq buf1 (match-string-no-properties 1))
             (forward-line 1)))
    (setq start-1 (point))
    (if (not (re-search-forward marker-regex (point-max) t))
        (error "Only 1 msgstr found")
      (setq buf2 (match-string-no-properties 1)
            end-1 (match-beginning 0))
      (let ((oldbuf (current-buffer)))
        (save-current-buffer
          (set-buffer (get-buffer-create
                       (generate-new-buffer-name buf1)))
          (erase-buffer)
          (insert-buffer-substring oldbuf start-1 end-1))

        (setq start-2 (point))
        (save-excursion
          ;; check for a third variant; if found ignore it
          (if (re-search-forward marker-regex (point-max) t)
              (setq end-2 (match-beginning 0))
            (setq end-2 (goto-char (1- (point-max))))))
        (save-current-buffer
          (set-buffer (get-buffer-create
                       (generate-new-buffer-name buf2)))
          (erase-buffer)
          (insert-buffer-substring oldbuf start-2 end-2))

        (if (not (string-equal (buffer-substring-no-properties start-1 end-1)
                               (buffer-substring-no-properties start-2 end-2)))
            (k-po-ediff-buffers-exit-recursive buf1 buf2 oldbuf end-2)
          (message "Variants are equal; delete %s" buf1)
          (forward-line -1)
          (delete-region (point-min) (point)))))))

(defun k-po-subedit-abort ()
  "Exit the subedit buffer, merely discarding its contents."
  (interactive)
  (let* ((edit-buffer (current-buffer))
         (back-pointer k-po-subedit-back-pointer)
         (entry-marker (nth 0 back-pointer))
         (overlay-info (nth 2 back-pointer))
         (k-po-subedit--entry-buffer (marker-buffer entry-marker)))
    (if (null k-po-subedit--entry-buffer)
        (error "Corresponding PO buffer does not exist anymore")
      (unless (or k-po-subedit--reused? (one-window-p))
        (delete-window))
      (switch-to-buffer k-po-subedit--entry-buffer)
      (goto-char entry-marker)
      (and overlay-info (k-po-dehighlight overlay-info))
      (kill-buffer edit-buffer)
      (setq k-po-edited-fields (delete back-pointer k-po-edited-fields)))))

(defun k-po-subedit-exit ()
  "Exit the subedit buffer, replacing the string in the PO buffer."
  (interactive)
  (goto-char (point-max))
  (skip-chars-backward " \t\n")
  (when (eq (preceding-char) ?<)
    (delete-region (1- (point)) (point-max)))
  (run-hooks 'k-po-subedit-exit-hook)
  (let ((str (buffer-string)))
    (k-po-subedit-abort)
    (let ((entry (k-po-current-entry)))
      (cond ((= (point) (k-po-entry-msgid-start entry))
             (k-po-set-comment str entry)
             (k-po-redisplay))
            ((= (point) (k-po-entry-msgstr-form-start entry))
             (when (k-po-set-msgstr-form str entry)
               (k-po-maybe-delete-previous-untranslated))
             (when k-po-auto-unfuzzy-on-edit
               (k-po-unfuzzy)))
            (t (debug))))))

;; TODO: tear down po-mode's own subedit code and use edit-indirect instead
(defun k-po-edit-string (string type expand-tabs)
  "Prepare a pop up buffer for editing STRING, which is of a given TYPE.
TYPE may be `comment' or `msgstr'.  If EXPAND-TABS, expand tabs to spaces.
Run functions on k-po-subedit-mode-hook."
  (let ((marker (make-marker)))
    (set-marker marker (cond ((eq type 'comment) k-po-start-of-msgid)
                             ((eq type 'msgstr) k-po-start-of-msgstr-form)))
    (if (k-po-check-for-pending-edit marker)
        (let ((edit-buffer (generate-new-buffer
                            (concat "*" (buffer-name) "*")))
              (edit-coding buffer-file-coding-system)
              (buffer (current-buffer))
              (window-count-before-switch (count-windows))
              overlay slot)
          ;; When editing the msgstr, highlight it in the source buffer
          (if (and (eq type 'msgstr))
              ;; ;; Try showing all of msgid in the upper window while editing.
              ;; (goto-char (1- k-po-start-of-msgstr-block))
              ;; (recenter -1)
              (save-excursion
                (goto-char k-po-start-of-entry)
                (re-search-forward k-po-any-msgid-regexp nil t)
                (let ((end (1- (match-end 0))))
                  (goto-char (match-beginning 0))
                  (re-search-forward "msgid +" nil t)
                  (setq overlay (k-po-create-overlay))
                  (k-po-highlight overlay (point) end buffer))))
          (setq slot (list marker edit-buffer overlay)
                k-po-edited-fields (cons slot k-po-edited-fields))
          (pop-to-buffer edit-buffer)
          (text-mode)
          (when (= window-count-before-switch (count-windows))
            (setq-local k-po-subedit--reused? t))
          (setq-local k-po-subedit-back-pointer slot)
          (setq-local indent-line-function
                      'indent-relative)
          (setq buffer-file-coding-system edit-coding)
          (setq local-abbrev-table k-po-mode-abbrev-table)
          (erase-buffer)
          (insert string)
          (goto-char (point-min))
          (and expand-tabs (setq indent-tabs-mode nil))
          ;; HACK We're already doing major mode things below
          (setq-local major-mode 'k-po-subedit-mode)
          (use-local-map k-po-subedit-mode-map)
          (when (fboundp 'easy-menu-define)
            (easy-menu-define k-po-subedit-mode-menu k-po-subedit-mode-map ""
              k-po-subedit-mode-menu-layout))
          ;; HACK: this shouldn't be necessary if we're using edit-indirect
          (when (and (fboundp 'evil-insert-state)
                     (= (buffer-size) 0))
            (evil-insert-state))
          (set-syntax-table k-po-subedit-mode-syntax-table)
          (run-hooks 'k-po-subedit-mode-hook)
          (message "%s" (substitute-command-keys k-po-subedit-message))))))

(defun k-po-edit-comment ()
  "Use another window to edit the current translator comment."
  (interactive)
  (k-po-edit-string (k-po-entry-comment (k-po-current-entry))
                    'comment
                    nil))

(defun k-po-edit-comment-and-ediff ()
  "Use `ediff' to edit the current translator comment.
This function calls `k-po-edit-msgstr' and `k-po-subedit-ediff'; for more info
read `k-po-subedit-ediff' documentation."
  (interactive)
  (k-po-edit-comment)
  (k-po-subedit-ediff))

(defun k-po-edit-msgstr ()
  "Use another window to edit the current msgstr."
  (interactive)
  (let ((entry (k-po-current-entry)))
    (k-po-edit-string (if (and k-po-auto-edit-with-msgid
                               (k-po-entry-type? entry 'untranslated))
                          (k-po-entry-msgid entry)
                        (k-po-entry-msgstr entry))
                      'msgstr
                      t)))

(defun k-po-edit-msgstr-and-ediff ()
  "Use `ediff' to edit the current msgstr.
This function calls `k-po-edit-msgstr' and `k-po-subedit-ediff'; for more info
read `k-po-subedit-ediff' documentation."
  (interactive)
  (k-po-edit-msgstr)
  (k-po-subedit-ediff))

;;; String normalization and searching.

(defun k-po-normalize-old-style (explain)
  "Normalize old gettext style fields using K&R C multiline string syntax.
To minibuffer messages sent while normalizing, add the EXPLAIN string."
  (let ((here (point-marker))
        (counter 0))
    (goto-char (point-min))
    (message "Normalizing %d, %s" counter explain)
    (while (re-search-forward
            "\\(^#?[ \t]*msg\\(id\\|str\\)[ \t]*\"\\|[^\" \t][ \t]*\\)\\\\\n"
            nil t)
      (if (= (% counter 10) 0)
          (message "Normalizing %d, %s" counter explain))
      (replace-match "\\1\"\n\"" t nil)
      (setq counter (1+ counter)))
    (goto-char here)
    (message "Normalizing %d...done" counter)))

(defun k-po-normalize-field (field explain)
  "Normalize FIELD of all entries.  FIELD is either the symbol msgid or msgstr.
To minibuffer messages sent while normalizing, add the EXPLAIN string."
  (let ((here (point-marker))
        (counter 0)
        entry)
    (goto-char (point-min))
    (while (re-search-forward k-po-any-msgstr-block-regexp nil t)
      (when (= (% counter 10) 0)
        (message "Normalizing %d, %s" counter explain))
      (goto-char (match-beginning 0))
      (setq entry (k-po-current-entry))
      (cond ((eq field 'msgid) (k-po-set-msgid
                                (k-po-entry-msgid entry)
                                entry))
            ((eq field 'msgstr) (k-po-set-msgstr-form
                                 (k-po-entry-msgstr entry)
                                 entry)))
      (goto-char (k-po-entry-end entry))
      (setq counter (1+ counter)))
    (goto-char here)
    (message "Normalizing %d...done" counter)))

;; Normalize, but the British way! :-)
(defsubst k-po-normalise () (k-po-normalize))

(defun k-po-normalize ()
  "Normalize all entries in the PO file."
  (interactive)
  (k-po-normalize-old-style "pass 1/3")
  ;; FIXME: This cannot work: t and nil are not msgid and msgstr.
  (k-po-normalize-field t "pass 2/3")
  (k-po-normalize-field nil "pass 3/3")
  ;; The last PO file entry has just been processed.
  (if (not (= k-po-end-of-entry (point-max)))
      (kill-region k-po-end-of-entry (point-max)))
  ;; A bizarre format might have fooled the counters, so recompute
  ;; them to make sure their value is dependable.
  (k-po-compute-counters nil))

;;; Original program sources as context.

(defun k-po-show-source-path ()
  "Echo the current source search path in the message area."
  (if k-po-search-path
      (let ((cursor k-po-search-path)
            string)
        (while cursor
          (setq string (concat string (if string " ") (car (car cursor)))
                cursor (cdr cursor)))
        (message string))
    (message "Empty source path.")))

(defun k-po-consider-source-path (directory)
  "Add a given DIRECTORY, requested interactively, to the source search path."
  (interactive "DDirectory for search path: ")
  (setq k-po-search-path (cons (list (if (string-match "/$" directory)
                                         directory
                                       (concat directory "/")))
                               k-po-search-path))
  (setq k-po-reference-check 0)
  (k-po-show-source-path))

(defun k-po-ignore-source-path ()
  "Delete a directory, selected with completion, from the source search path."
  (interactive)
  (setq k-po-search-path
        (delete (list (completing-read "Directory to remove? "
                                       k-po-search-path nil t))
                k-po-search-path))
  (setq k-po-reference-check 0)
  (k-po-show-source-path))

(defun k-po-ensure-source-references ()
  "Extract all references into a list, with paths resolved, if necessary."
  (k-po-find-span-of-entry)
  (if (= k-po-start-of-entry k-po-reference-check)
      nil
    (setq k-po-reference-alist nil)
    (save-excursion
      (goto-char k-po-start-of-entry)
      (if (re-search-forward "^#:" k-po-start-of-msgid t)
          (let (current name line path file)
            (while (looking-at "\\(\n#:\\)? *\\([^: ]*\\):\\([0-9]+\\)")
              (goto-char (match-end 0))
              (setq name (match-string-no-properties 2)
                    line (match-string-no-properties 3)
                    path k-po-search-path)
              (if (string-equal name "")
                  nil
                (while (and (not (file-exists-p
                                  (setq file (concat (car (car path)) name))))
                            path)
                  (setq path (cdr path)))
                (setq current (and path file)))
              (if current
                  (setq k-po-reference-alist
                        (cons (list (concat current ":" line)
                                    current
                                    (string-to-number line))
                              k-po-reference-alist)))))))
    (setq k-po-reference-alist (nreverse k-po-reference-alist)
          k-po-reference-cursor k-po-reference-alist
          k-po-reference-check k-po-start-of-entry)))

(defun k-po-show-source-context (triplet)
  "Show the source context given a TRIPLET which is (PROMPT FILE LINE)."
  (find-file-other-window (car (cdr triplet)))
  (goto-line (car (cdr (cdr triplet))))
  (other-window 1)
  (let ((maximum 0)
        position
        (cursor k-po-reference-alist))
    (while (not (eq triplet (car cursor)))
      (setq maximum (1+ maximum)
            cursor (cdr cursor)))
    (setq position (1+ maximum)
          k-po-reference-cursor cursor)
    (while cursor
      (setq maximum (1+ maximum)
            cursor (cdr cursor)))
    (message "Displaying %d/%d: \"%s\"" position maximum (car triplet))))

(defun k-po-cycle-source-reference ()
  "Display some source context for the current entry.
If the command is repeated many times in a row, cycle through contexts."
  (interactive)
  (k-po-ensure-source-references)
  (if k-po-reference-cursor
      (k-po-show-source-context
       (car (if (eq last-command 'k-po-cycle-source-reference)
                (or (cdr k-po-reference-cursor) k-po-reference-alist)
              k-po-reference-cursor)))
    (error "No resolved source references")))

(defun k-po-select-source-reference ()
  "Select one of the available source contexts for the current entry."
  (interactive)
  (k-po-ensure-source-references)
  (if k-po-reference-alist
      (k-po-show-source-context
       (assoc
        (completing-read "Which source context? " k-po-reference-alist nil t)
        k-po-reference-alist))
    (error "No resolved source references")))


;;; Miscellaneous features.

(defun k-po-statistics ()
  "Say how many entries in each category, and the current position."
  (interactive)
  (k-po-compute-counters t))

(defun k-po-validate ()
  "Use `msgfmt' for validating the current PO file contents."
  (interactive)
  ;; The 'compile' subsystem is autoloaded through a call to (compile ...).
  ;; We need to initialize it outside of any binding. Without this statement,
  ;; all defcustoms and defvars of compile.el would be undone when the let*
  ;; terminates.
  (require 'compile)
  (let* ((dev-null
          (cond ((boundp 'null-device) null-device) ; since Emacs 20.3
                ((memq system-type '(windows-nt windows-95)) "NUL")
                (t "/dev/null")))
         (output
          (if k-po-keep-mo-file
              (concat (file-name-sans-extension buffer-file-name) ".mo")
            dev-null))
         (compilation-buffer-name-function
          (lambda (mode)
            (concat "*" mode " validation*")))
         (compile-command (concat k-po-msgfmt-program
                                  " --statistics -c -v -o "
                                  (shell-quote-argument output) " "
                                  (shell-quote-argument buffer-file-name))))
    (k-po-msgfmt-version-check)
    (compile compile-command)))

(defvar k-po-msgfmt-version-checked nil)
(defun k-po-msgfmt-version-check ()
  "`msgfmt' from GNU gettext 0.10.36 or greater is required."
  (with-temp-buffer
    (or
     ;; Don't bother checking again.
     k-po-msgfmt-version-checked

     (and
      ;; Make sure 'msgfmt' is available.
      (condition-case nil
          (call-process k-po-msgfmt-program
                        nil t nil "--verbose" "--version")
        (file-error nil))

      ;; Make sure there's a version number in the output:
      ;; 0.11 or 0.10.36 or 0.19.5.1 or 0.11-pre1 or 0.16.2-pre1
      (progn (goto-char (point-min))
             (or (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)[-_A-Za-z0-9]+$")
                 (looking-at ".* \\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)[-_A-Za-z0-9]+$")))

      ;; Make sure the version is recent enough.
      (>= (string-to-number
           (format "%d%03d%03d"
                   (string-to-number (match-string 1))
                   (string-to-number (match-string 2))
                   (string-to-number (or (match-string 3) "0"))))
          010036)

      ;; Remember the outcome.
      (setq k-po-msgfmt-version-checked t))

     (error "`msgfmt' from GNU gettext 0.10.36 or greater is required"))))

(defun k-po-guess-archive-name ()
  "Return the ideal file name for this PO file in the central archives."
  (let ((filename (file-name-nondirectory buffer-file-name))
        start-of-header end-of-header package version team)
    (save-excursion
      ;; Find the PO file header entry.
      (goto-char (point-min))
      (re-search-forward k-po-any-msgstr-block-regexp)
      (setq start-of-header (match-beginning 0)
            end-of-header (match-end 0))
      ;; Get the package and version.
      (goto-char start-of-header)
      (if (re-search-forward "\n\
\"Project-Id-Version: \\(GNU \\|Free \\)?\\([^\n ]+\\) \\([^\n ]+\\)\\\\n\"$"
                             end-of-header t)
          (setq package (match-string-no-properties 2)
                version (match-string-no-properties 3)))
      (if (or (not package) (string-equal package "PACKAGE")
              (not version) (string-equal version "VERSION"))
          (error "Project-Id-Version field does not have a proper value"))
      ;; File name version and Project-Id-Version must match
      (cond (;; A `filename' w/o package and version info at all
             (string-match "^[^\\.]*\\.po\\'" filename))
            (;; TP Robot compatible `filename': PACKAGE-VERSION.LL.po
             (string-match (concat (regexp-quote package)
                                   "-\\(.*\\)\\.[^\\.]*\\.po\\'") filename)
             (if (not (equal version (match-string-no-properties 1 filename)))
                 (error "\
Version mismatch: file name: %s; header: %s.\n\
Adjust Project-Id-Version field to match file name and try again"
                        (match-string-no-properties 1 filename) version))))
      ;; Get the team.
      (if (stringp k-po-force-team-code)
          (setq team k-po-force-team-code)
        (goto-char start-of-header)
        (if (re-search-forward "\n\
\"Language-Team: \\([^ ].*[^ ]\\) <.+@.+>\\\\n\"$"
                               end-of-header t)
            (let ((name (match-string-no-properties 1)))
              (if name
                  (let ((pair (assoc name k-po-team-name-to-code)))
                    (if pair
                        (setq team (cdr pair))
                      (setq team (read-string (format "\
Team name '%s' unknown.  What is the team code? "
                                                      name)))))))))
      (if (or (not team) (string-equal team "LL"))
          (error "Language-Team field does not have a proper value"))
      ;; Compose the name.
      (concat package "-" version "." team ".po"))))

(defun k-po-guess-team-address ()
  "Return the team address related to this PO file."
  (let (team)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward k-po-any-msgstr-block-regexp)
      (goto-char (match-beginning 0))
      (if (re-search-forward
           "\n\"Language-Team: +\\(.*<\\(.*\\)@.*>\\)\\\\n\"$"
           (match-end 0) t)
          (setq team (match-string-no-properties 2)))
      (if (or (not team) (string-equal team "LL"))
          (error "Language-Team field does not have a proper value"))
      (match-string-no-properties 1))))

(defun k-po-send-mail ()
  "Start composing a letter, possibly including the current PO file."
  (interactive)
  (let* ((team-flag (y-or-n-p
                     "\
Write to your team (y) or the Translation Project bot (n)? "))
         (address (if team-flag
                      (k-po-guess-team-address)
                    k-po-translation-project-address)))
    (if (not (y-or-n-p "Include current PO file in mail? "))
        (apply k-po-compose-mail-function address
               (read-string "Subject? ") nil)
      (when (buffer-modified-p)
        (error "The file has not been saved, and it has not been validated"))
      (when (and (y-or-n-p "Have you validated ('V') this file? ")
                 (or (zerop k-po-untranslated-counter)
                     (y-or-n-p
                      (format "%d entries are untranslated, include anyway? "
                              k-po-untranslated-counter)))
                 (or (zerop k-po-fuzzy-counter)
                     (y-or-n-p
                      (format "%d entries are still fuzzy, include anyway? "
                              k-po-fuzzy-counter)))
                 (or (zerop k-po-obsolete-counter)
                     (y-or-n-p
                      (format "%d entries are obsolete, include anyway? "
                              k-po-obsolete-counter))))
        (let ((buffer (current-buffer))
              (name (k-po-guess-archive-name))
              (transient-mark-mode nil)
              (coding-system-for-read buffer-file-coding-system)
              (coding-system-for-write buffer-file-coding-system))
          (apply k-po-compose-mail-function address
                 (if team-flag
                     (read-string "Subject: ")
                   (format "%s %s" k-po-translation-project-mail-label name))
                 nil)
          (goto-char (point-min))
          (re-search-forward
           (concat "^" (regexp-quote mail-header-separator) "\n"))
          (save-excursion
            (save-restriction
              (narrow-to-region (point) (point))
              (insert-buffer-substring buffer)
              (shell-command-on-region
               (point-min) (point-max)
               (concat k-po-gzip-uuencode-command " " name ".gz") t t)))))))
  (message ""))

(defun k-po-confirm-and-quit ()
  "Confirm if quit should be attempted and then, do it.
This is a failsafe.  Confirmation is asked if only the real quit would not."
  (interactive)
  (when (k-po-check-all-pending-edits)
    (when (or (buffer-modified-p)
              (> k-po-untranslated-counter 0)
              (> k-po-fuzzy-counter 0)
              (> k-po-obsolete-counter 0)
              (y-or-n-p "Really quit editing this PO file? "))
      (k-po-quit))
    (message "")))

(defun k-po-quit ()
  "Save the PO file and kill buffer.
However, offer validation if appropriate and ask confirmation if untranslated
strings remain."
  (interactive)
  (when (k-po-check-all-pending-edits)
    (let ((quit t))
      ;; Offer validation of newly modified entries.
      (when (and (buffer-modified-p)
                 (not (y-or-n-p
                       "File was modified; skip validation step? ")))
        (message "")
        (k-po-validate)
        ;; If we knew that the validation was all successful, we should
        ;; just quit.  But since we do not know yet, as the validation
        ;; might be asynchronous with PO mode commands, the safest is to
        ;; stay within PO mode, even if this implies that another
        ;; 'k-po-quit' command will be later required to exit for true.
        (setq quit nil))
      ;; Offer to work on untranslated entries.
      (when (and quit
                 (or (> k-po-untranslated-counter 0)
                     (> k-po-fuzzy-counter 0)
                     (> k-po-obsolete-counter 0))
                 (not (y-or-n-p
                       "Unprocessed entries remain; quit anyway? ")))
        (setq quit nil)
        (k-po-auto-select-entry))
      ;; Clear message area.
      (message "")
      ;; Or else, kill buffers and quit for true.
      (when quit
        (save-buffer)
        (kill-buffer (current-buffer))))))

(defun k-po-compile (po mo)
  "Compile PO file to MO (a path)."
  (interactive
   (let* ((po (if (derived-mode-p 'k-po-mode)
                  buffer-file-name
                (read-file-name "Source PO file: ")))
          (mo (if current-prefix-arg
                  (f-join (read-directory-name "Place compiled MO at: ")
                          (concat (f-base po) ".mo"))
                (f-join (xdg-data-home)
                        "locale"
                        "zh_TW"
                        "LC_MESSAGES"
                        (concat (f-base po) ".mo")))))
     (list po mo)))
  (make-directory (f-dirname mo) t)
  (let (status output)
    (with-temp-buffer
      (setq status (call-process "msgfmt" nil t nil
                                 "-o" mo po))
      (setq output (string-trim (buffer-string))))
    (if (= 0 status)
        (message "Compiled %s to %s" po mo)
      (message "Something went wrong. See *k/po* for output")
      (with-current-buffer (get-buffer-create "*k/po*")
        (erase-buffer)
        (insert (format "%s" output))))))

(defun k-po-check-dir (dir)
  "Check all po files within DIR."
  (interactive "DCheck directory of PO files: ")
  (with-current-buffer (get-buffer-create "*k/po*")
    (erase-buffer)
    (insert (format "Errors in %s:\n" dir))
    (start-process "k/po - find" (current-buffer)
                   "find" (expand-file-name dir)
                   "-path" "*.po"
                   "-execdir"
                   "msgfmt" "{}" "-o" "/dev/null" "--check-format" ";")
    (display-buffer (current-buffer))))

(defun k-po-memory-bulk-fill-dir-msgstr (dir &optional func)
  "Do `k-po-memory-bulk-fill-msgstr' for each file in DIR.
FUNC is passed to `k-po-memory-bulk-fill-msgstr', which see."
  (interactive "DPO Directory: ")
  (let* ((files (directory-files-recursively dir (rx ".po" eos) nil nil t))
         (i 0)
         (total (length files)))
    (message "Applying translation memory for files...")
    (dolist (file files)
      (cl-incf i)
      (message "Applying translation memory for files (%s/%s)... (%s)"
               i total
               (file-relative-name file dir))
      (let ((k-po-insert-memory nil)
            (inhibit-message t))
        (with-current-buffer (find-file-noselect file)
          (k-po-memory-bulk-fill-msgstr func)
          (basic-save-buffer))))
    (message "Applying translation memory for files...done")))

(defun k-po-memory-bulk-fill-msgstr (&optional func)
  "For untranslated entries with just one matching TM entry, apply that TM entry.

If there are multiple TM entries, but the top entry has over 100
occurrences in the translation memory, then that TM entry is also applied.

The expectation is that a review of the file will be done later.

The condition for TM entries described above (only one entry or
top has > 100 occurrences) can be overriden by passing a function
in as FUNC. For each PO entry, this function receives one
argument, its matching TM entries (as `k-po-memory-entry'
objects), and should return a string to be used as the
translation of the PO entry."
  (interactive)
  (goto-char (point-min))
  (let ((target-lang (k-po-current-target-language)))
    (k-po-map-entries
     (lambda (entry)
       (catch 'continue
         (when (memq (k-po-entry-type entry)
                     '(translated obsolete))
           (throw 'continue t))
         (let* ((msgid (k-po-entry-msgid entry))
                (tm (k-po-memory-get msgid target-lang))
                new-target)
           ;; No memory entry
           (unless tm
             (throw 'continue t))
           (setq new-target
                 (if func
                     (funcall func tm)
                   (and (or
                         ;; if no more than one entry
                         ;; -> if just one entry, since the zero entry case
                         ;; would've already continued
                         (not (> (length tm) 1))
                         ;; or if the first entry has over 100 occurrences
                         (> (k-po-memory-entry-count (car tm))
                            100))
                        (k-po-memory-entry-target (car tm)))))
           (unless new-target
             (throw 'continue t))
           (message "Setting entry at %s to %s" (point) new-target)
           (k-po-set-msgstr-form new-target entry)
           (k-po--unfuzzy entry)))))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.po[tx]?\\'\\|\\.po\\." . k-po-mode))

(provide 'k-po-mode)

;;; k-po-mode.el ends here
