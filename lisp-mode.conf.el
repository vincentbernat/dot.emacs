;;; Code:

(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)

;; Inline results when evaluating sexps
;; Reference: http://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html

(autoload 'cider--make-result-overlay "cider-overlays")

(defun vbe/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
                              :where point
                              :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (vbe/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (vbe/eval-overlay r (point))))

(advice-add 'eval-defun :filter-return
            (lambda (r)
              (vbe/eval-overlay
               r
               (save-excursion
                 (end-of-defun)
                 (point)))))

;;; lisp-mode.conf.el ends here
