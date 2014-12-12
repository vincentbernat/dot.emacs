;; Do not mangle `message-dont-reply-to-names' on followups.
(ad-disable-advice 'gnus-summary-followup 'before
		   'gnus-identities:gnus-summary-followup)
(ad-activate 'gnus-summary-followup)
;; Posting styles definition
(setq gnus-posting-styles
      '((".*"
         (x-identity "default")
         (name "Vincent Bernat")
         (address "bernat@luffy.cx")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@bernat.im"))
         (x-identity "bernat.im")
         (address "vincent@bernat.im")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@crans.org"
                                "*@*.crans.org"
                                "*@crans.ens-cachan.fr"
                                "crans.*" "tac.*"
                                "*@ens-cachan.fr"))
         (x-identity "crans")
         (address "bernat@crans.org")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@debian.org"
                                "*@*.debian.org"
                                "*@debian.ch"
                                "*@*.debian.ch"
                                "*@debconf.org"
                                "*@*.debconf.org"))
         (x-identity "debian")
         (eval (vbe:gnus/will-sign-message))
         (address "bernat@debian.org")
         (organization "Debian")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@veltigroup.com"
                                "*@exoscale.ch"))
         (x-identity "exoscale")
         (address "Vincent.Bernat@exoscale.ch")
         (organization "exoscale")
         (gcc "nnimap+exoscale:Sent")
         (signature (mapconcat 'identity
                               '("Vincent Bernat — Vincent.Bernat@exoscale.ch"
                                 "❬❱ http://www.exoscale.ch")
                               "\n")))
        ((header "subject" "RFS: ")
         (signature (mapconcat 'identity
                               '("Debian package sponsoring guidelines:"
                                 " http://vincent.bernat.im/en/debian-package-sponsoring.html")
                               "\n")))
        ((vbe:mail-related-to '("*@enxio.fr" "*@enx.io"))
         (x-identity "enxio")
         (address "bernat@enx.io")
         (organization "ENXIO")
         (signature "Vincent Bernat ☯ https://enx.io"))))

(defun vbe:gnus/will-sign-message ()
  "Setup a local hook to make the article signed."
  (add-hook 'gnus-message-setup-hook
	    'mml-secure-message-sign-pgpmime t t))

;; Signature
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
