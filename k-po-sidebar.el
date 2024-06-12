;;; k-po-sidebar.el --- Sidebar buffer for info display -*- lexical-binding: t -*-

;;; Commentary:

;; A sidebar buffer for displaying information about an entry.
;; Based on Org-roam v1's sidebar implementation.

;;; Code:

(require 'k-po-view)
(require 'k-po-entry)
(require 'k-po-memory)

(declare-function k-po-set-msgstr-form "k-po-mode")
(declare-function k-po-current-target-language "k-po-mode")
(declare-function k-po-jump-to-entry "k-po-mode")

(defcustom k-po-sidebar-position 'right
  "Where the data sidebar should be placed.

Valid values are

- `left',
- `right',
- `top',
- `bottom',

or a function returning one of the above."
  :type '(choice (const left)
                 (const right)
                 (const top)
                 (const bottom)
                 function)
  :group 'k-po)

(defcustom k-po-sidebar-width 0.33
  "Width of the data sidebar.

Has an effect if and only if `k-po-sidebar-position' is `left' or `right'."
  :type 'number
  :group 'k-po)

(defcustom k-po-sidebar-height 0.27
  "Height of the data sidebar.

Has an effect if and only if `k-po-sidebar-position' is `top' or `bottom'."
  :type 'number
  :group 'k-po)

(defcustom k-po-sidebar-buffer-name "*k-po-info*"
  "Name of the sidebar buffer."
  :type 'string
  :group 'k-po)

(defcustom k-po-sidebar-window-parameters nil
  "Additional window parameters for the sidebar window."
  :type '(alist)
  :group 'k-po)

(defcustom k-po-sidebar-widgets
  '(stats other-languages translation-memory)
  "Widgets to be shown in the sidebar.

Values are either functions that insert the widget into the
sidebar buffer, or symbols that, when added onto the prefix
\"k-po-sidebar--widget--\", refer to an existing function that
does the same.

Each function receives three inputs: the msgid of the current
entry, the target language of the current file, and the source
file buffer."
  :type '(repeat symbol)
  :group 'k-po)

(defvar k-po-sidebar--current-file nil
  "The file the sidebar is currently displaying info for.")

(defvar k-po-sidebar--last-window nil
  "The last window the sidebar was toggled from.")

(defun k-po-sidebar--set-width (width)
  "Set the width of the current window to WIDTH."
  (unless (one-window-p)
    (let ((window-size-fixed nil)
          (w (max width window-min-width)))
      (enlarge-window-horizontally (- w (window-width))))))

(defun k-po-sidebar--set-height (height)
  "Set the height of the current window to HEIGHT."
  (unless (one-window-p)
    (let ((window-size-fixed nil)
          (h (max height window-min-width)))
      (enlarge-window (- h (window-width))))))

(defun k-po-sidebar--visible? ()
  "Return whether the sidebar is currently visible."
  (get-buffer-window k-po-sidebar-buffer-name))

(defun k-po-sidebar--buffer-init ()
  "Initialize the sidebar buffer."
  (variable-pitch-mode)
  (setq-local word-wrap nil)
  (setq-local truncate-lines nil))

(defun k-po-sidebar--insert-heading (string)
  "Insert STRING as a heading."
  (insert
   (faceup-render-string
    (format "«k:#» «B:%s»\n" string))
   "\n"))

(defun k-po-sidebar--widget--stats (_msgid _target-lang source-buffer)
  "Insert the stats of the entry.
MSGID, TARGET-LANG, and SOURCE-BUFFER are passed in from the update function."
  (k-po-sidebar--insert-heading "Stats")
  (insert (with-current-buffer source-buffer
            (faceup-render-string
             (format "translated: %s/%s (%.2f%%)\nfuzzy: %s\nuntranslated: %s\nobsolete: %s"
                     k-po-translated-counter
                     (+ k-po-translated-counter
                        k-po-fuzzy-counter
                        k-po-untranslated-counter)
                     (* 100
                        (/ k-po-translated-counter
                           (+ k-po-translated-counter
                              k-po-fuzzy-counter
                              k-po-untranslated-counter)
                           1.0))
                     k-po-fuzzy-counter
                     k-po-untranslated-counter
                     k-po-obsolete-counter)))))

(defun k-po-sidebar--widget--other-languages (msgid target-lang _source-buffer)
  "Insert the other languages widget.
MSGID, TARGET-LANG, and SOURCE-BUFFER are passed in from the update function."
  (k-po-sidebar--insert-heading "Other languages")
  (let ((mapping (k-po-memory--rows-count-group
                  (k-po-memory--select
                   "SELECT target, target_lang
FROM mapping
WHERE source = ? AND NOT target_lang = ?
LIMIT 5"
                   msgid target-lang))))
    (if (not mapping)
        (insert (propertize "None" 'face 'italic))
      (insert
       (string-join
        (mapcar (pcase-lambda (`(,target ,lang))
                  (faceup-render-string
                   (format "«B:%s» «I:%s»"
                           target
                           (car (rassoc lang k-po-team-name-to-code)))))
                mapping)
        "\n")))))

(defun k-po-sidebar--widget--translation-memory (msgid target-lang source-buffer)
  "Insert the translation memory languages widget.
MSGID, TARGET-LANG, and SOURCE-BUFFER are passed in from the update function."
  (k-po-sidebar--insert-heading "Translation Memory")
  (let ((mapping (k-po-memory-get msgid target-lang)))
    (if (not mapping)
        (insert (propertize "None" 'face 'italic))
      (pcase-dolist (`(,source ,target ,count) mapping)
        (insert (propertize
                 (format "(%sx) %s\n"
                         count
                         target)
                 'face 'bold))
        (k-po-view--insert-button "Use" nil
          (with-current-buffer source-buffer
            (k-po-set-msgstr-form target)))
        (insert "\n")
        (k-po-view--insert-button "Copy" nil
          (k-po-view--copy target))
        (let ((files (remove (buffer-file-name source-buffer)
                             (k-po-memory--get-files source target))))
          (when files
            (insert "\n")
            (insert-text-button
             "Visit file"
             'face 'button
             'action (lambda (&rest _)
                       (k-po-memory--jump-to-file-entry source target files)))))
        (insert "\n\n")))))

(defun k-po-sidebar--buffer-update (source-buffer)
  "Update the sidebar buffer contents for SOURCE-BUFFER.
SOURCE-BUFFER is the PO file buffer."
  (with-current-buffer k-po-sidebar-buffer-name
    (let ((inhibit-read-only t)
          msgid target-lang)
      (with-current-buffer source-buffer
        (let ((entry (k-po-current-entry)))
          (setq target-lang (k-po-current-target-language))
          (setq msgid (k-po-entry-msgid entry))))
      (erase-buffer)
      ;; Special case for header
      (if (equal msgid "")
          (progn
            (insert (faceup-render-string
                     (format "«B:%s»\n\n"
                             (file-name-nondirectory
                              (buffer-file-name source-buffer)))))
            (k-po-sidebar--widget--stats msgid target-lang source-buffer))
        (insert (faceup-render-string
                 (format "«B:%s»\n\n" msgid)))
        (let ((i 0)
              (l (length k-po-sidebar-widgets)))
          (dolist (widget k-po-sidebar-widgets)
            (cl-incf i)
            (catch 'continue
              (cond ((functionp widget)
                     (funcall widget
                              msgid target-lang source-buffer))
                    ((functionp (intern-soft (format "k-po-sidebar--widget--%s" widget)))
                     (funcall (intern-soft (format "k-po-sidebar--widget--%s" widget))
                              msgid target-lang source-buffer))
                    (t
                     (message "(k-po-mode) Warning: %s is not a sidebar widget" widget)
                     (throw 'continue t)))
              (unless (>= i l)
                (insert "\n\n")))))))))

(defun k-po-sidebar--post-command-h ()
  "Hook function for updating the sidebar buffer in `post-command-hook'."
  (when (and (k-po-sidebar--visible?)
             (derived-mode-p 'k-po-mode))
    (ignore-errors
      (k-po-sidebar--buffer-update (window-buffer)))))

(defun k-po-sidebar-toggle ()
  "Toggle display of the sidebar."
  (interactive)
  (setq k-po-sidebar--last-window (get-buffer-window))
  (if (k-po-sidebar--visible?)
      ;; If visible: hide it
      (delete-window (get-buffer-window k-po-sidebar-buffer-name))
    ;; If not visible: show it
    ;; Set up the window
    (let ((position (if (functionp k-po-sidebar-position)
                        (funcall k-po-sidebar-position)
                      k-po-sidebar-position)))
      (save-selected-window
        (select-window
         (display-buffer-in-side-window
          (get-buffer-create k-po-sidebar-buffer-name)
          `((side \, position)
            (window-parameters \, k-po-sidebar-window-parameters))))
        (pcase position
          ((or 'right 'left)
           (k-po-sidebar--set-width
            (round (* (frame-width)  k-po-sidebar-width))))
          ((or 'top  'bottom)
           (k-po-sidebar--set-height
            (round (* (frame-height) k-po-sidebar-height)))))))
    ;; Set up the window
    (with-current-buffer k-po-sidebar-buffer-name
      (k-po-sidebar--buffer-init))))

(provide 'k-po-sidebar)

;;; k-po-sidebar.el ends here
