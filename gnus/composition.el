;; Citation format
(setq message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format " ❦ %e %B %Y %R %Z, %f :\n")

;; Ability to reply on top (shame, shame)...
(defun vbe/gnus/wide-reply-on-top (n)
  "Wide reply on top of the current message"
  (interactive "P")
  (let ((message-cite-reply-position 'above)
	(message-citation-line-format
	 (mapconcat 'identity
		    (delq nil
			  `(" ――――――― Original Message ―――――――"
			    " From: %f"
			    " Sent: %e %B %Y %R %Z"
			    ,(concat " Subject: " (gnus-with-article-headers
						    (mail-fetch-field "Subject")))
			    ,(vbe/gnus/extract-names "To")
			    ,(vbe/gnus/extract-names "Cc")
			    ""))
		    "\n")))
    (gnus-summary-wide-reply-with-original n)))

(defun vbe/gnus/extract-names (field)
  "Extract the list of names from a given field"
  (let ((result (gnus-with-article-headers
	    (mapconcat '(lambda (x) (or (first x)
					(second x)))
		       (mail-extract-address-components
			(or (mail-fetch-field field)
			    "") t) "; "))))
    (when (> (length result) 0)
      (concat " " field ": " result))))

(define-key gnus-summary-mode-map (kbd "v f") 'vbe/gnus/wide-reply-on-top)


(provide 'vbe/gnus/composition)
