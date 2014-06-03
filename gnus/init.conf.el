;; No .newsrc
(setq gnus-directory (vbe:run-directory "gnus")
      gnus-home-directory gnus-directory
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
      gnus-startup-file (nnheader-concat gnus-home-directory "newsrc"))

;; Search with nnir
(require 'nnir)

;; Servers to use
(setq gnus-select-method
      ;; Primary server: IMAP
      `(nnimap ""
               (nnimap-address "imap.luffy.cx")
               (nnimap-authenticator login)
               (nnir-search-engine imap)
               (nnimap-stream tls)))
(setq gnus-agent nil)

;; SMTP
(defun vbe:message-send-mail ()
  (let ((server-and-port))
    (save-restriction
      (message-narrow-to-headers)
      (mail-fetch-field "X-SMTP-Server") ":")
    (cond ((stringp server-and-port)
           ;; We need to use smtpmail-send-it
           (message-remove-header "X-SMTP-Server")
           (let* ((splitted (split-string server-and-port ":"))
                  (smtpmail-smtp-server (car splitted))
                  (smtpmail-smtp-port (number-to-string (car (cdr splitted)))))
             (smtpmail-send-it)))
          (t (message-send-mail-with-sendmail)))))
(setq message-send-mail-function 'vbe:message-send-mail)

;; How to archive sent messages
(setq gnus-message-archive-group '((cond ((message-news-p) nil)
                                         ((vbe:at 'deezer) nil)
                                         (t "Sent")))
      gnus-message-archive-method "nnimap:"
      gnus-update-message-archive-method t

;; Where to store local mails (drafts, ...)
      message-directory (nnheader-concat gnus-home-directory "mail")

;; How to grab old articles
      gnus-refer-article-method
      '(current				; Local server first
	(nntp "news.gmane.org")))	; gmane

(setq gnus-secondary-select-methods
      `((nnimap "exoscale"
                (nnimap-address "localhost")
                (nnimap-server-port 1143)
                (nnimap-authenticator login)
                (nnimap-stream network)
                (nnir-search-engine imap))
        (nndraft ""
                 (nndraft-directory ,(nnheader-concat message-directory "drafts")))))

;; Scan news every 5 minutes if idle for more than 30 seconds
(gnus-demon-add-handler 'gnus-demon-scan-news 5 30)

(require 'spam)
(setq spam-install-hooks t)
(spam-initialize)
;; The spam is handled according to groups configuration.
