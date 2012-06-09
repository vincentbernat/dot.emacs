(defun vbe/gnus-bbdb-init ()
  "bbdb initialization"
  (require 'bbdb-mua)
  (require 'bbdb-gnus)
  (bbdb-initialize 'gnus 'message)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
  (bbdb-insinuate-gnus)
  (setq bbdb-north-american-phone-numbers-p nil ; don't use US format
	bbdb-dwim-net-address-allow-redundancy t; always display full name
	bbdb-user-mail-address-re vbe/mail-addresses
	bbdb-notice-mail-hook 'bbdb-auto-notes-hook) ; auto-fill "Notes"

  ; What to set in "Notes"?
  (setq bbdb-auto-notes-alist
      (list
       '("Organization"
         (".*" company 0))
       '("Subject"
         (".*" subjects 0))
       '("Newsgroups"
          ("[^,]+" newsgroups 0))
       '("Xref"
         ("[^ ]+ \\([^ :]+\\):[0-9]+" newsgroups 1))
       '("User-Agent"
         (".*" mailer 0))
       '("X-Mailer"
         (".*" mailer 0))
       '("X-Newsreader"
         (".*" mailer 0))))

  ; Hack to get last 5 subjects
  (put 'subjects 'field-separator "\n")
  (add-hook 'bbdb-notice-hook 'vbe/bbdb-trim-subjects)
  (defun vbe/bbdb-trim-subjects (record)
    "Remove all but the first 5 lines from the subjects
    in the notes field of a BBDB record. Meant to be
    added to bbdb-change-hook."
    (let* ((sep (get 'subjects 'field-separator))
	   (foo (reverse
		 (split-string
		  (or (bbdb-record-getprop record 'subjects) "")
		  sep)))
	   (num-to-keep 5)
	   (new-subj ""))
      (while (and (> num-to-keep 0) (> (length foo) 0))
	(let ((subj (mail-decode-encoded-word-string (car foo))))
	  (if (and (> (length subj) 0)
		   (not
		    (string= (encode-coding-string subj 'utf-8)
			     (encode-coding-string new-subj 'utf-8))))
	      (setq new-subj (concat (encode-coding-string subj 'iso-8859-15)
				     (if (> (length new-subj) 0)
					 (concat sep new-subj)
				       ""))
		    num-to-keep (- num-to-keep 1)))
	  (setq foo (cdr foo))))
      (bbdb-record-putprop record 'subjects new-subj))))

(vbe/add-package (list :name "bbdb"
		       :init '(vbe/gnus-bbdb-init)))

(provide 'vbe/gnus-bbdb)
