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
      mm-discouraged-alternatives '("text/html"
                                    "text/richtext"
                                    "multipart/related"))

;; Better visibility of HTML emails
(setq shr-color-visible-distance-min 10
      shr-color-visible-luminance-min 60)

;; Visible headers
(setq gnus-visible-headers
      (mapcar #'(lambda (header) (format "^%s:" header))
	      (split-string (mapconcat 'identity
				       '("From Organization Subject Newsgroups"
					 "To Cc Reply-To Followup-To Mail-Followup-To"
                                         "Thread-Topic"
					 "X-Mailer X-Newsreader"
                                         "User-Agent X-Posting-Agent"
					 "X-Spam-Level" "Date") " "))))

;; Gravatar
(require 'gnus-gravatar)
(defun vbe:gnus-treat-from-gravatar ()
  "Display gravatar, only when online."
  ;; We do that because DNS requests in Emacs is not asynchronous and
  ;; moreover, even without any route, we may end up waiting for the
  ;; timeout...
  (when (vbe:working-network-connection?)
    (gnus-treat-from-gravatar)))
(add-hook 'gnus-article-prepare-hook 'vbe:gnus-treat-from-gravatar)
