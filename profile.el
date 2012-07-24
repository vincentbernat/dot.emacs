(setq vbe:profile
      (cond ((string-match (concat (regexp-quote ".p.fti.net") "$")
			   (system-name))
	     "orange")
	    (t "unknown")))

(defun vbe:at (where)
  "Return `t' if the current profile is WHERE."
  (string= vbe:profile
	   (cond ((symbolp where) (symbol-name where))
		 ((stringp where) where)
		 (t ""))))

(provide 'vbe:profile)
