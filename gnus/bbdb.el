(defun vbe/gnus/bbdb-init ()
  "bbdb initialization"
  (bbdb-initialize 'gnus 'message)
  (bbdb-insinuate-gnus)
  (bbdb-mua-auto-update-init nil 'search) ; Only update existing
					  ; records, don't create new
					  ; ones automatically

  (setq bbdb-message-pop-up nil	      ; Display BBDB record but not always
	bbdb-pop-up-window-size 0.8   ; Maximum size of the BBDB popup
	;; When using ':' in summary, ask to create the record if it
	;; does not exist
	bbdb-mua-update-interactive-p '(query . query)
	bbdb-phone-style nil)	      ; Don't assume a phone style

  ;; Add notes when updating a record
  (add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)
  ;; Display the record when it exists
  (add-hook 'gnus-article-prepare-hook 'vbe/gnus/bbdb-display-record)

  ;; What to set in "Notes"?
  (setq bbdb-auto-notes-rules
      (list
       '("Organization"
         (".*" organization "\\1" nil))
       '("Subject"
         (".*" subjects vbe/gnus/bbdb-subject-canonicalize nil))
       '("Newsgroups"
          ("[^,]+" newsgroups identity nil))
       '("Xref"
         ("[^ ]+ \\([^ :]+\\):[0-9]+" newsgroups "\\1" nil))
       '("User-Agent"
         (".*" mailer identity nil))
       '("X-Mailer"
         (".*" mailer identity nil))
       '("X-Newsreader"
         (".*" mailer identity nil))))
  ;; Start clean
  (setq bbdb-auto-notes-rules-expanded nil))

(defun vbe/gnus/bbdb-subject-canonicalize (subject)
  "Canonicalize SUBJECT."
  (let ((newsubject
	 (message-strip-subject-trailing-was
	  (message-strip-subject-encoded-words
	   (message-strip-subject-re
	    (mail-decode-encoded-word-string subject))))))
    newsubject))

(defun vbe/gnus/bbdb-display-record ()
  "Display appropriate BBDB record for the current message."
  (unless
      (bbdb-mua-display-records nil 'search)
    ;; No record found, close the BBDB popup
    (let ((window (get-buffer-window bbdb-buffer-name)))
      (when window (delete-window window)))))
  

(vbe/add-package (list :name "bbdb"
		       :init '(vbe/gnus/bbdb-init)))

(provide 'vbe/gnus/bbdb)
