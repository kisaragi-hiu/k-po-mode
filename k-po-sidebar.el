;;; k-po-sidebar.el --- Sidebar buffer for info display -*- lexical-binding: t -*-

;;; Commentary:

;; A sidebar buffer for displaying information about an entry.
;; Based on Org-roam v1's sidebar implementation.

;;; Code:

(require 'k-po-entry)
(require 'k-po-memory)

(declare-function k-po-current-entry "k-po-mode")
(declare-function k-po-set-msgstr-form "k-po-mode")
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

(defun k-po-sidebar--buffer-update (source-buffer)
  "Update the sidebar buffer contents for SOURCE-BUFFER.
SOURCE-BUFFER is the PO file buffer."
  (with-current-buffer k-po-sidebar-buffer-name
    (let ((inhibit-read-only t)
          msgid)
      (with-current-buffer source-buffer
        (let ((entry (k-po-current-entry)))
          (setq msgid (k-po-entry-msgid entry))))
      (erase-buffer)
      ;; Root section
      (insert (propertize "Translation Memory\n" 'face 'bold))
      (insert "\n")
      (let ((mapping (k-po-memory-get msgid)))
        (if (not mapping)
            (insert (propertize "None" 'face 'italic))
          (pcase-dolist (`(,source ,target ,count) mapping)
            (insert (propertize
                     (format "%s\n(%sx) %s\n"
                             source
                             count
                             target)
                     'face 'bold))
            (insert-text-button
             "Use this translation"
             'face 'button
             'action (lambda (&rest _)
                       (with-current-buffer source-buffer
                         (k-po-set-msgstr-form target))))
            (insert "\n")
            (insert-text-button
             "Copy"
             'face 'button
             'action (lambda (&rest _)
                       (kill-new target)
                       (message "Copied \"%s\"..." target)))
            (let ((files (remove (buffer-file-name source-buffer)
                                 (k-po-memory--get-files source target))))
              (when files
                (insert "\n")
                (insert-text-button
                 "Visit file"
                 'face 'button
                 'action (lambda (&rest _)
                           (if (= 1 (length files))
                               (find-file (car files))
                             (find-file (completing-read "Which file: " files)))
                           (k-po-jump-to-entry source target)))))
            (insert "\n\n")))))))

(defun k-po-sidebar--post-command-h ()
  "Hook function for updating the sidebar buffer in `post-command-hook'."
  (when (k-po-sidebar--visible?)
    (k-po-sidebar--buffer-update (window-buffer))))

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
