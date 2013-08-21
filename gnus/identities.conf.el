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
		(organization "Dailymotion")
		(signature (mapconcat 'identity
				      '("Vincent Bernat | vincent.bernat@dailymotion.com"
					"Dailymotion | 140 Boulevard Malesherbes | 75017 Paris | France")
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
				       "*@*.debian.org"
                                       "*@debconf.org"
                                       "*@*.debconf.org"))
		(x-identity "debian")
		(eval (vbe:gnus/will-sign-message))
		(address "bernat@debian.org")
		(organization "Debian"))
               ((header "subject" "RFS: ")
                (signature (mapconcat 'identity
                                      '("Debian package sponsoring guidelines:"
                                        " http://vincent.bernat.im/en/debian-package-sponsoring.html")
                                      "\n")))
               ((vbe:mail-related-to '("*@dailymotion.com"))
                (x-identity "dailymotion")
		(name "Vincent Bernat")
		(address "vincent.bernat@dailymotion.com")
		(organization "Dailymotion")
                (gcc "nnimap:Boulot.Dailymotion.Sent")
		(signature (mapconcat 'identity
				      '("Vincent Bernat | vincent.bernat@dailymotion.com"
					"Dailymotion | 140 Boulevard Malesherbes | 75017 Paris | France")
				      "\n")))
	       ((vbe:mail-related-to '("*@enxio.fr" "*@enx.io"))
		(x-identity "enxio")
		(address "bernat@enx.io")
		(organization "ENXIO")
		(signature "Vincent Bernat ☯ https://enx.io"))))))

(defun vbe:gnus/will-sign-message ()
  "Setup a local hook to make the article signed."
  (add-hook 'gnus-message-setup-hook
	    'mml-secure-message-sign-pgpmime t t))
