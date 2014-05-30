;; No .newsrc
(setq gnus-directory (vbe:run-directory "gnus")
      gnus-home-directory gnus-directory
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
      gnus-startup-file (nnheader-concat gnus-home-directory "newsrc"))

;; Search with nnir
(require 'nnir)

;; Servers to use
(cond ((vbe:at 'deezer)
       (setq gnus-select-method
             `(nnimap ""
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl))
             message-send-mail-function 'smtpmail-send-it
             smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
             smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                          "vbe@deezer.com" nil))
             smtpmail-default-smtp-server "smtp.gmail.com"
             smtpmail-smtp-server "smtp.gmail.com"
             smtpmail-smtp-service 587
             gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"))
      (t
       (setq gnus-select-method
             ;; Primary server: IMAP
             `(nnimap ""
                      (nnimap-address "imap.luffy.cx")
                      (nnimap-authenticator login)
                      (nnir-search-engine imap)
                      (nnimap-stream tls))
             message-send-mail-function 'message-send-mail-with-sendmail)))
(setq gnus-agent nil)

;; How to archive sent messages
(setq gnus-message-archive-group '((cond ((message-news-p) nil)
                                         ((vbe:at 'deezer) nil)
                                         (t "Sent")))
      gnus-message-archive-method "nnimap:"
      gnus-update-message-archive-method t

;; Where to store local mails (drafts, ...)
      message-directory (nnheader-concat gnus-home-directory "Mail")
      nnfolder-directory (nnheader-concat message-directory "archive")

;; How to grab old articles
      gnus-refer-article-method
      '(current				; Local server first
	(nntp "news.gmane.org")))	; gmane


;; Scan news every 5 minutes if idle for more than 30 seconds
(gnus-demon-add-handler 'gnus-demon-scan-news 5 30)

(require 'spam)
(setq spam-install-hooks t)
(spam-initialize)
;; The spam is handled according to groups configuration.
