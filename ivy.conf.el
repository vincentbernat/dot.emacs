(diminish 'ivy-mode)

(defun vbe:ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer with an unicode arrow prefix."
  (ivy--format-function-generic
   (lambda (str)
     (concat "▶ " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))

(setq ivy-use-virtual-buffers nil
      ivy-count-format "(%d/%d) "
      ivy-extra-directories nil
      ivy-format-function #'vbe:ivy-format-function-arrow)

;; Alias for S-SPC
(define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-restrict-to-matches)
