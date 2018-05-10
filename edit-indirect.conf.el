(require 's)
(require 'dash)

(defvar edit-indirect--left-margin 0)

(defun vbe:compute-left-margin (code)
  "Compute left margin of a string of code."
  (-min
   (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
         (-remove 's-blank? (s-lines code)))))

(defun vbe:after-indirect-edit-remove-left-margin ()
  "Remove left-margin and save it into a local variable."
  (let ((lm (vbe:compute-left-margin (buffer-substring (point-min) (point-max)))))
    (indent-rigidly (point-min) (point-max) (* -1 lm))
    (setq-local edit-indirect--left-margin lm)))

(defun vbe:after-indirect-edit-restore-left-margin ()
  "Restore left-margin before commiting."
  (indent-rigidly (point-min) (point-max) edit-indirect--left-margin))

(add-hook 'edit-indirect-after-creation-hook #'vbe:after-indirect-edit-remove-left-margin)
(add-hook 'edit-indirect-before-commit-hook #'vbe:after-indirect-edit-restore-left-margin)
