;; Citation format
(setq message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format " ❦ %e %B %Y %R %Z, %f :\n")

;; Increase score of followups
(setq gnus-kill-files-directory (vbe:run-directory "gnus/scores"))
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;; Prefer 8-bit encoding
(add-to-list 'mm-content-transfer-encoding-defaults '("text/plain" 8bit))

;; Use the list of subscribed addresses for MFT from the group/topic parameters
(setq message-subscribed-address-functions
      '(gnus-find-subscribed-addresses))
(setq gnus-parameters
      '(("^OS\\.Debian\\."
	 (subscribed . t))))

;; Sign messages
(require 'epg)
(setq mml2015-use 'epg		 ; use epg
      mm-verify-option 'always	 ; always check for sigs
      mm-decrypt-option 'always  ; always decrypt
      auth-source-gpg-encrypt-to user-mail-address)

(setq gnus-signature-limit 12.0)	; No more than 12 lines for a signature

;; Set user name and email address
(setq user-full-name "Vincent Bernat"
      user-mail-address (cond ((vbe:at 'deezer) "vbe@deezer.com")
			      (t "bernat@luffy.cx")))
(setq vbe:mail-addresses
      (mapcar #'(lambda (name)
		 (format "\\(^\\|[^.]\\)\\b%s[@\\.]" name))
	      (apply 'append (mapcar 'split-string
				     '("bernat vbernat vincent.bernat"
                                       "vbe"
				       "Vincent.Bernat"))))
      gnus-ignored-from-addresses vbe:mail-addresses  ; When to display To: instead of From:
      message-dont-reply-to-names
      (append vbe:mail-addresses
	      (mapcar 'regexp-quote
		      '("@noreply.github.com"
                        "notifications@github.com"
                        "control@bugs.debian.org"
                        "submit@bugs.debian.org")))) ; Addresses to prune on wide reply

(setq message-valid-fqdn-regexp
      (format "\\(%s\\|lanexpert\\.grp\\)" message-valid-fqdn-regexp))

(require 'dired)
(defun vbe:mail-related-to (what &optional fields)
  "Determine if the current message has something to do with WHAT.
It will search in FIELDS (default `To', `Cc' and `From') to check
if any of the given expressions in WHAT is present."
  (when (buffer-live-p gnus-summary-buffer)
    (gnus-with-article-buffer
      (let ((what (if (listp what) what (list what)))
	    (matched nil)
	    (fields (if fields fields '("to" "from" "cc" "newsgroups"))))
	(dolist (field (mapcar 'message-fetch-field fields))
	  (when field
	    (dolist (address (delq nil (mapcar 'second
					       (mail-extract-address-components
						field t))))
	      (dolist (w what)
		(message (dired-glob-regexp w))
		(when (string-match (dired-glob-regexp w) address)
		  (add-to-list 'matched address))))))
	(when matched t)))))

(require 'gnus-identities)
