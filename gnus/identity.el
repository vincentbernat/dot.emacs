(vbe/require 'profile)

;; Set user name and email address
(setq user-full-name "Vincent Bernat"
      user-mail-address (cond ((vbe/at 'orange) "vincent.bernat@wanadooportails.com")
			      (t "bernat@luffy.cx")))
(setq vbe/mail-addresses (mapcar '(lambda (name)
				    (format "^%s@" name))
				 (apply 'append	(mapcar 'split-string
							'("bernat vbernat vincent.bernat"
							  "Vincent.Bernat vbernat.ext")))))

(setq gnus-ignored-from-addresses vbe/mail-addresses  ; When to display To: instead of From:
      message-dont-reply-to-names vbe/mail-addresses) ; Addresses to prune on wide reply

(defun vbe/mail-related-to (what &optional fields)
  "Determine if the current message has something to do with WHAT.
It will search in FIELDS (default `To', `Cc' and `From') to check
if any of the given expressions in WHAT is present."
  (when (get-buffer gnus-article-buffer)
    (save-excursion
      (set-buffer gnus-article-buffer)
      (let ((what (if (listp what) what (list what)))
	    (matched nil)
	    (fields (if fields fields '("to" "from" "cc"))))
	(dolist (field (mapcar 'message-fetch-field fields))
	  (when field
	    (dolist (address (delq nil (mapcar 'second
					       (mail-extract-address-components
						field t))))
	      (dolist (w what)
		(when (string-match (concat "\\.*" (regexp-quote w)) address)
		  (add-to-list 'matched address))))))
	(when matched t)))))

(defun vbe/init-identities ()
  "Initialize identity module"
  (require 'gnus-identities)
  ;; Do not mangle `message-dont-reply-to-names' on followups.
  (ad-disable-advice 'gnus-summary-followup 'before 'gnus-identities:gnus-summary-followup)
  (ad-activate 'gnus-summary-followup)
  ;; Posting styles definition
  (setq gnus-posting-styles
	(cond ((vbe/at 'orange)
	 '((".*"
	    (x-identity "default")
	    (name "Vincent Bernat")
	    (address "vincent.bernat@wanadooportails.com")
	    (organization "OF/DMGP/Portail/DOP/Hébex/Bagnolet")
	    (signature (mapconcat 'identity
				  '("Vincent Bernat"
				    ""
				    "Société Freelance.com"
				    "pour Orange ☯ OF/DMGP/Portail/DOP/Hébex/Bagnolet"
				    "✆ +33 1 58 96 62 69"
				    "✉ vbernat.ext@orange.com")
				  "\n")))))
	      (t
	       '((".*"
		  (x-identity "default")
		  (name "Vincent Bernat")
		  (address "bernat@luffy.cx")
		  (signature (fortune)))
		 ((vbe/mail-related-to '("@bernat.im"))
		  (x-identity "bernat.im")
		  (address "vincent@bernat.im"))
		 ((vbe/mail-related-to '("@debian.org"
					 "@lists.debian.org"
					 "@bugs.debian.org"))
		  (x-identity "debian")
		  (address "bernat@debian.org")
		  (organization "Debian"))
		 ((vbe/mail-related-to '("@enxio.fr" "@enx.io"))
		  (x-identity "enxio")
		  (address "bernat@enx.io")
		  (organization "ENXIO")
		  (signature "Vincent Bernat ☯ https://enx.io")))))))

(vbe/add-package (list :name "gnus-identities"
		       :init '(vbe/init-identities)))

(provide 'vbe/gnus/identity)
