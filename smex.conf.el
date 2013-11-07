(setq smex-save-file (expand-file-name "run/smex-items"
                                       user-emacs-directory))

(defadvice smex (around space-inserts-hyphen activate compile)
  "Insert an hyphen when using space. This mimics default M-x
  behaviour."
  (let ((ido-cannot-complete-command
         `(lambda ()
            (interactive)
            (if (string= " " (this-command-keys))
                (insert ?-)
              (funcall ,ido-cannot-complete-command)))))
    ad-do-it))
