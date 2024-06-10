;;; k-po-view.el --- Buffer view primatives -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'cl-lib)

(defvar-local k-po-view--local nil
  "A local space for storing arbitrary buffer-local data.")

(defmacro k-po-view--insert-button (label arguments &rest body)
  "Insert a text button with LABEL that, when clicked, runs BODY.

Buttons inserted with this command are styled like ones that run
commands.

ARGUMENTS is a plist containing the button\\='s properties. Keys can
be given as keywords, they will be converted to symbols."
  (declare (indent 2))
  (cl-with-gensyms (args-sym)
    `(let ((,args-sym
            ;; values in ARGUMENTS should be evaluated
            (list ,@arguments)))
       (unless (plist-get ,args-sym 'type)
         (setq ,args-sym (plist-put ,args-sym 'type 'button)))
       (setq ,args-sym (plist-put ,args-sym 'action (lambda (&rest _) ,@body)))
       (apply #'insert-text-button (format "%s" ,label) ,args-sym))))

(defun k-po-view--copy (str)
  "Copy STR into the clipboard and show some feedback."
  (kill-new str)
  (message "Copied \"%s\"" str))

(provide 'k-po-view)

;;; k-po-view.el ends here
