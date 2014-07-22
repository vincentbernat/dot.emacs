(add-to-list 'ac-dictionary-directories (vbe:run-directory "ac-dict"))
(setq ac-comphist-file (expand-file-name "run/ac-comphist.dat"
                                         user-emacs-directory))

(require 'auto-complete-config)
(ac-config-default)

(setq ac-use-quick-help nil)            ;; It's slow

;; Don't use up/down arrow (use M-n, M-p only) to browse list
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)

(defvar vbe:ac-flycheck-poll-completion-end-timer nil
  "Timer to poll end of completion.")
(defun vbe:ac-syntax-checker-workaround ()
  (make-variable-buffer-local 'vbe:ac-flycheck-poll-completion-end-timer)
  (when (require 'flycheck nil t)
    (defadvice flycheck-handle-idle-change (around vbe:ac-flycheck-stop-advice activate)
      (if ac-completing
          (setq vbe:ac-flycheck-poll-completion-end-timer
                (run-at-time 0.5
                             nil
                             #'flycheck-handle-idle-change))
        ad-do-it)))
    (when (featurep 'flycheck)
      (ad-disable-advice 'flycheck-handle-idle-change 'around 'vbe:ac-flycheck-stop-advice)))
(vbe:ac-syntax-checker-workaround)
