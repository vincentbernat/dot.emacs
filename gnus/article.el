;; Hide too long citations
(setq gnus-treat-hide-citation t
      gnus-treat-highlight-citation t
      gnus-article-skip-boring t
      gnus-cited-lines-visible '(3 . 6))

;; Buttons
(setq gnus-inhibit-mime-unbuttonizing nil ; Display some buttons
      gnus-buttonized-mime-types '("multipart/alternative"
				   "multipart/signed"
				   "multipart/encrypted")
      mm-discouraged-alternatives '("text/html" "text/richtext"))

;; Visible headers
(setq gnus-visible-headers 
      (mapcar '(lambda (header) (format "^%s:" header))
	      (split-string (mapconcat 'identity
				       '("From Organization Subject Newsgroups"
					 "To Cc Reply-To Followup-To Mail-Followup-To"
					 "X-Mailer X-Newsreader User-Agent X-Posting-Agent"
					 "X-Spam-Level" "Date") " "))))

;; Gravatar
(require 'gnus-gravatar)
(add-hook 'gnus-article-prepare-hook 'gnus-treat-from-gravatar)

(provide 'vbe/gnus/article)
