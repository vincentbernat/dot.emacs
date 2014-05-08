;; Citation format
(setq message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format " ❦ %e %B %Y %R %Z, %f :\n")

;; Increase score of followups
(setq gnus-kill-files-directory (vbe:run-directory "gnus/scores"))
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

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

;; Signature
(setq message-signature 'vbe:fortune)
(defconst vbe:fortune-program nil
  "*Program used to generate epigrams, default \"fortune\".")

(defvar vbe:fortune-switches (list "-e"
			       "50%" (expand-file-name "~/.sigs/kernelcookies")
			       "50%" (expand-file-name "~/.sigs/prog-style"))
  "*List of extra arguments when `vbe:fortune-program' is invoked")

(defun vbe:fortune (&optional long-p)
  "Generate a random epigram.
An optional prefix argument generates a long epigram.
The epigram is inserted at point if called interactively."
  (interactive "*P")
  (let ((fortune-buffer (generate-new-buffer " fortune"))
        (fortune-string "Have an adequate day."))
    (unwind-protect
        (save-excursion
          (set-buffer fortune-buffer)
          (apply 'call-process
                 (append (list (or vbe:fortune-program "fortune") nil t nil)
                         vbe:fortune-switches
                         (list (if long-p "-l" "-s"))))
          (skip-chars-backward "\n\t ")
          (setq fortune-string (buffer-substring (point-min) (point))))
      (kill-buffer fortune-buffer))
    (if (interactive-p)
        (insert fortune-string))
    fortune-string))

(setq gnus-signature-limit 12.0)	; No more than 12 lines for a signature

;; Set user name and email address
(setq user-full-name "Vincent Bernat"
      user-mail-address (cond ((vbe:at 'deezer) "vbe@deezer.com")
			      (t "bernat@luffy.cx")))
(setq vbe:mail-addresses
      (mapcar '(lambda (name)
		 (format "^%s[@\\.]" name))
	      (apply 'append (mapcar 'split-string
				     '("bernat vbernat vincent.bernat"
                                       "vbe"
				       "Vincent.Bernat")))))

(setq gnus-ignored-from-addresses vbe:mail-addresses  ; When to display To: instead of From:
      message-dont-reply-to-names
      (append vbe:mail-addresses
	      (mapcar 'regexp-quote
		      '("@noreply.github.com"
                        "notifications@github.com"
                        "control@bugs.debian.org"
                        "submit@bugs.debian.org")))) ; Addresses to prune on wide reply

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
