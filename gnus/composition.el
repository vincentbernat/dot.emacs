;; Citation format
(setq message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format " ❦ %e %B %Y %R %Z, %f :\n")

;; Ability to reply on top (shame, shame)...
(defun vbe:gnus/wide-reply-on-top (n)
  "Wide reply on top of the current message"
  (interactive "P")
  (vbe:gnus/reply-somehow-on-top n 'gnus-summary-wide-reply-with-original))
(defun vbe:gnus/reply-on-top (n)
  "Reply on top of the current message"
  (interactive "P")
  (vbe:gnus/reply-somehow-on-top n 'gnus-summary-reply-with-original))

(defun vbe:gnus/reply-somehow-on-top (n how)
  "Reply using HOW on top of the current message"
  (let ((message-cite-reply-position 'above)
	(message-citation-line-format
	 (mapconcat 'identity
		    (delq nil
			  `(" ――――――― Original Message ―――――――"
			    " From: %f"
			    " Sent: %e %B %Y %R %Z"
			    ,(concat " Subject: " (gnus-with-article-headers
						    (mail-fetch-field "Subject")))
			    ,(vbe:gnus/extract-names "To")
			    ,(vbe:gnus/extract-names "Cc")
			    ""))
		    "\n")))
    (funcall how n)))

(defun vbe:gnus/extract-names (field)
  "Extract the list of names from a given field"
  (let ((result (gnus-with-article-headers
	    (mapconcat '(lambda (x) (or (first x)
					(second x)))
		       (mail-extract-address-components
			(or (mail-fetch-field field)
			    "") t) "; "))))
    (when (> (length result) 0)
      (concat " " field ": "
	      (with-temp-buffer
		(insert result)
		(fill-region (point-min) (point-max))
		(replace-string "\n" (concat "\n"
					     (make-string (+ 3 (length field))
							  ? ))
				nil (point-min) (point-max))
		(buffer-substring (point-min) (point-max)))))))

(define-key gnus-summary-mode-map (kbd "f") 'vbe:gnus/wide-reply-on-top)
(define-key gnus-summary-mode-map (kbd "r") 'vbe:gnus/reply-on-top)

;; Increase score of followups
(setq gnus-kill-files-directory (vbe:run-directory "gnus/scores"))
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;; Use the list of subscribed addresses for MFT from the group/topic parameters
(setq message-subscribed-address-functions
      '(gnus-find-subscribed-addresses))
(setq gnus-parameters
      '(("^OS\\.Debian\\."
	 (subscribed . t))))

(provide 'vbe:gnus/composition)
