;; Do not mangle `message-dont-reply-to-names' on followups.
(ad-disable-advice 'gnus-summary-followup 'before
		   'gnus-identities:gnus-summary-followup)
(ad-activate 'gnus-summary-followup)
;; Posting styles definition
(setq gnus-posting-styles
      (cond ((vbe:at 'dailymotion)
	     '((".*"
		(x-identity "default")
		(name "Vincent Bernat")
		(address "vincent.bernat@dailymotion.com")
		(organization "DailyMotion")
		(signature (mapconcat 'identity
				      '("Vincent Bernat"
					""
					"✉ vincent.bernat@dailymotion.com")
				      "\n")))))
	    (t
	     '((".*"
		(x-identity "default")
		(name "Vincent Bernat")
		(address "bernat@luffy.cx")
		(signature (vbe:fortune)))
	       ((vbe:mail-related-to '("*@bernat.im"))
		(x-identity "bernat.im")
		(address "vincent@bernat.im"))
	       ((vbe:mail-related-to '("*@crans.org"
				       "*@*.crans.org"
				       "*@crans.ens-cachan.fr"
				       "crans.*" "tac.*"
				       "*@ens-cachan.fr"))
		(x-identity "crans")
		(address "bernat@crans.org"))
	       ((vbe:mail-related-to '("*@debian.org"
				       "*@*.debian.org"))
		(x-identity "debian")
		(eval (vbe:gnus/will-sign-message))
		(address "bernat@debian.org")
		(organization "Debian"))
	       ((vbe:mail-related-to '("*@enxio.fr" "*@enx.io"))
		(x-identity "enxio")
		(address "bernat@enx.io")
		(organization "ENXIO")
		(signature "Vincent Bernat ☯ https://enx.io"))))))

(defun vbe:gnus/will-sign-message ()
  "Setup a local hook to make the article signed."
  (add-hook 'gnus-message-setup-hook
	    'mml-secure-message-sign-pgpmime t t))
