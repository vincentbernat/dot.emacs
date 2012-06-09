;; Servers to use
(setq gnus-select-method '(nntp "news.free.fr")
      gnus-secondary-select-methods '((nnimap ""
					      (nnimap-address "imap.luffy.cx")
					      (nnimap-authenticator login)
					      (nnir-search-engine imap)
					      (nnimap-stream tls))
				      (nntp "news.crans.org"))
      message-send-mail-function 'message-send-mail-with-sendmail
      gnus-agent nil)
			 

;; How to archive sent messages
(setq gnus-message-archive-group '((unless (message-news-p)
				     "Sent"))
      gnus-message-archive-method '(nnimap ""))

;; Hack to circumvent some problem with new async code in Gnus
;; See: http://git.gnus.org/cgit/gnus.git/commit/?id=8eb5f1cde674860ba1a05c8754e45dbadc137ca5
(defun vbe/gnus-hack-startup ()
  (let ((funcs (nthcdr 3 (assoc 'nntp
				nnoo-definition-alist))))
    (setcar funcs (delq 'nntp-finish-retrieve-group-infos
			(delq 'nntp-retrieve-group-data-early (car funcs))))))
(add-hook 'gnus-started-hook 'vbe/gnus-hack-startup)

;; Scan news every 5 minutes if idle for more than 30 seconds
(gnus-demon-add-handler 'gnus-demon-scan-news 5 30)

(provide 'vbe/gnus-servers)
