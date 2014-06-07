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
      ;; Primary server: dovecot to offlineimap
      `(nnimap ""
               (nnimap-stream shell)
               (nnimap-shell-program "/usr/lib/dovecot/imap")
               (nnir-search-engine imap)))

(require 'offlineimap)
(add-hook 'gnus-before-startup-hook 'offlineimap)
(setq gnus-agent nil)

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
      `((nndraft ""
                 (nndraft-directory ,(nnheader-concat message-directory "drafts")))))

;; Scan news every 5 minutes if idle for more than 30 seconds
(gnus-demon-add-handler 'gnus-demon-scan-news 5 30)

(require 'spam)
(setq spam-install-hooks t)
(spam-initialize)
;; The spam is handled according to groups configuration.
