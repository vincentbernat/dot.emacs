(vbe/require 'profile)

;; Servers to use
(setq gnus-select-method
      (cond ((vbe/at 'orange) '(nnimap ""
				       (nnimap-address "depotmail.infra.b1.p.fti.net")
				       (nnimap-authenticator login)
				       (nnir-search-engine imap)
				       (nnimap-stream ssl)))
	    (t '(nntp "news.free.fr")))
      gnus-secondary-select-methods
      (cond ((vbe/at 'orange) nil)
	    (t '((nnimap ""
			 (nnimap-address "imap.luffy.cx")
			 (nnimap-authenticator login)
			 (nnir-search-engine imap)
			 (nnimap-stream tls))
		 (nntp "news.crans.org"))))
      message-send-mail-function 'message-send-mail-with-sendmail
      gnus-agent nil)
			 

;; How to archive sent messages
(setq gnus-message-archive-group '((unless (message-news-p)
				     (cond ((vbe/at 'orange) "INBOX.Sent")
					   (t "Sent"))))
      gnus-message-archive-method '(nnimap ""))

;; Scan news every 5 minutes if idle for more than 30 seconds
(gnus-demon-add-handler 'gnus-demon-scan-news 5 30)

(provide 'vbe/gnus/servers)
