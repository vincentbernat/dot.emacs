(bbdb-initialize 'gnus 'message)
(bbdb-insinuate-gnus)
(bbdb-mua-auto-update-init nil 'update) ; Only update existing
					; records, don't create new
					; ones automatically

(setq bbdb-mua-pop-up nil	      ; Display BBDB record but not always
      bbdb-mua-pop-up-window-size 2   ; Maximum size of the BBDB popup
      ;; When using ':' in summary, ask to create the record if it
      ;; does not exist
      bbdb-mua-update-interactive-p '(query . query)
      bbdb-phone-style nil)	      ; Don't assume a phone style

;; Add notes when updating a record
(add-hook 'bbdb-notice-mail-hook 'bbdb-auto-notes)
;; Display the record when it exists
(eval-after-load "gnus-art"
  '(add-hook 'gnus-article-prepare-hook 'vbe:gnus/bbdb-display-record))

;; What to set in "Notes"?
(setq bbdb-auto-notes-rules
      (list
       '("Organization"
         (".*" organization "\\1" nil))
       '("Subject"
         (".*" subjects vbe:gnus/bbdb-subject-canonicalize nil))
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
(setq bbdb-auto-notes-rules-expanded nil)

;; Auto create some records otherwise, update existing records
(setq bbdb-update-records-p
      (lambda ()
  (let ((rest '(("From" . ".*@dailymotion.com")))
        done elt)
    (if (eq rest t)
        (setq done t)
      (while (and (setq elt (pop rest)) (not done))
        (dolist (header (if (stringp (car elt)) (list (car elt)) (car elt)))
          (if (bbdb-message-header-re header (cdr elt))
              (setq done t)))))
    (if invert (setq done (not done)))
    (if done 'create 'update))))

(defun vbe:gnus/bbdb-subject-canonicalize (subject)
  "Canonicalize SUBJECT."
  (let ((newsubject
	 (message-strip-subject-trailing-was
	  (message-strip-subject-encoded-words
	   (message-strip-subject-re
	    (mail-decode-encoded-word-string subject))))))
    newsubject))

(defun vbe:gnus/bbdb-display-record ()
  "Display appropriate BBDB record for the current message."
  (unless
      (bbdb-mua-display-records nil 'search)
    ;; No record found, close the BBDB popup
    (let ((window (get-buffer-window bbdb-buffer-name)))
      (when window (delete-window window)))))

;; We may also want to use EUDC.
(defun vbe:gnus/eudc-init (ldap)
  "Initialize EUDC subsystem.
Use LDAP as server. Can be one LDAP server or a list of LDAP servers.
"
  (require 'eudc)
  (let ((ldap (if (listp ldap) ldap (list ldap))))
    ;; Query first the current server, then the hotlist. The hotlist is
    ;; first LDAP, then localhost
    (setq eudc-inline-expansion-servers 'server-then-hotlist)
    ;; Servers
    (dolist (l ldap)
      (add-to-list 'eudc-server-hotlist `(,l . ldap) t))
    (add-to-list 'eudc-server-hotlist `("localhost" . bbdb) t)
    (if ldap
	(eudc-set-server (first ldap) 'ldap t)
      (eudc-set-server "localhost" 'bbdb t))
    ;; How completion should be done?
    (eudc-protocol-set 'eudc-inline-query-format
		       '((firstname)
			 (lastname)
			 (firstname lastname)
			 (mail))
		       'bbdb)
    (eudc-protocol-set 'eudc-inline-query-format
		       '((cn) (mail) (cn cn) (cn cn cn)
			 (sn) (givenName) (givenName sn)
			 (uid))
		       'ldap)
    ;; How to display results?
    (defalias 'bbdb-record-net 'bbdb-record-mail) ; Compatibility bbdbv3/v2
    (eudc-protocol-set 'eudc-inline-expansion-format
		       '("%s %s <%s>" firstname lastname net)
		       'bbdb)
    (eudc-protocol-set 'eudc-inline-expansion-format
		       '("%s <%s>" cn mail)
		       'ldap))

  ;; Define keyboard shortcut
  (eval-after-load "message"
    '(define-key message-mode-map (kbd "TAB")
       'vbe:gnus/eudc-expand-inline)))

;; Expand by adding a "*" at the end of the request
(defun vbe:gnus/eudc-expand-inline ()
  "EUDC expand using a wildcard."
  (interactive)
  (if (eq eudc-protocol 'ldap)
      (progn (move-end-of-line 1)
	     (insert "*")
	     (unless (condition-case nil
			 (eudc-expand-inline)
		       (error nil))
	       (backward-delete-char-untabify 1)))
    (eudc-expand-inline)))

(when (vbe:at 'orange)
  ;; Configure LDAP server
  (require 'ldap)
  (setq ldap-ldapsearch-args (quote ("-tt" "-LLL" "-x")))
  (add-to-list 'ldap-host-parameters-alist
	       '("ldap.infra.multis.p.fti.net"
		 base "ou=People,dc=fti,dc=net"
		 auth nil
		 scope subtree))
  (vbe:gnus/eudc-init "ldap.infra.multis.p.fti.net"))
