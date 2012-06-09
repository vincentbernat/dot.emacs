;; Hide too long citations
(setq gnus-treat-hide-citation t
      gnus-treat-highlight-citation t
      gnus-article-skip-boring t
      gnus-cited-lines-visible '(3 . 6))

;; Buttons
(setq gnus-inhibit-mime-unbuttonizing nil ; Display some buttons
      gnus-buttonized-mime-types '("text/.*"
				   "multipart/signed"
				   "multipart/encrypted"))

;; Visible headers
(setq gnus-visible-headers 
      (mapcar '(lambda (header) (format "^%s:" header))
	      (split-string (mapconcat 'identity
				       '("From Organization Subject Newsgroups"
					 "To Cc Reply-To Followup-To Mail-Followup-To"
					 "X-Mailer X-Newsreader User-Agent X-Posting-Agent"
					 "X-Spam-Level" "Date") " "))))

(provide 'vbe/gnus-article)

