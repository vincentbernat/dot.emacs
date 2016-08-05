(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(define-key goto-map "j" 'avy-goto-subword-1)
(define-key goto-map "M-j" 'avy-goto-subword-1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c '") 'vbe:edit-region-in-another-buffer)
(global-set-key (kbd "C-c |") #'align-current)
(global-set-key (kbd "C-c .") #'bm-toggle)
(global-set-key (kbd "C-c /") #'bm-next)
(global-set-key (kbd "C-c ,") #'bm-previous)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(global-unset-key "\C-x\C-c")           ; Too easy to hit by
                                        ; accident. Never used. Use
                                        ; kill-emacs directly
(global-unset-key (kbd "<insert>"))     ; on my X1, this key is mapped
                                        ; near other modifiers, this
                                        ; is easy to strike it by
                                        ; error

(when window-system
  (global-unset-key "\C-z"))

;; See: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun vbe:smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace/symbol character on this
line.  If point is already there, move to the beginning of the
line.  Effectively toggle between the first non-whitespace/symbol
character and the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    ;; Expanded from (back-to-indentation) but also skip symbols
    (beginning-of-line 1)
    (skip-syntax-forward " _" (line-end-position))
    (backward-prefix-chars)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'vbe:smarter-move-beginning-of-line)

(provide 'vbe/bindings)
