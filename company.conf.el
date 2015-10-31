;;; Code:

(diminish 'company-mode)

;; Don't use up/down arrow (use M-n, M-p only) to browse list
(define-key company-active-map [down] nil)
(define-key company-active-map [up] nil)

;;; company-mode.conf.el ends here
