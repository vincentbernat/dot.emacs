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
      ;; Primary server: dovecot
      `(nnimap ""
               (nnimap-stream shell)
               (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:~/.mbsync/mails/luffy")
               (nnir-search-engine imap)))
(setq message-send-mail-function 'message-send-mail-with-sendmail)
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
      `((nnimap "exoscale"
                (nnimap-stream shell)
                (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:~/.mbsync/mails/exoscale")
                (nnir-search-engine imap))
      (nndraft ""
                 (nndraft-directory ,(nnheader-concat message-directory "drafts")))))

(require 'auth-source)
(require 'comint)

;; Small wrapper around mbsync
(defun vbe:mbsync (channel &optional quick)
  "run the `mbsync` command asynchronously"
  (interactive "sChannel: \nP")
  (let* ((name (format "*mbsync-%s*" channel))
         (args (if quick (list (format "%s:INBOX" channel))
                 (list channel)))
         (previous (get-process name)))
    (if (and previous (process-live-p previous))
        (error "mbsync is already running")
      (let* ((process-environment (copy-sequence process-environment))
             (secrets (nth 0 (auth-source-search :max 1
                                                       :host (format "mbsync-%s" channel)
                                                       :require '(:user :secret))))
             (secret (plist-get secrets :secret))
             (dummy (setenv "USER" (plist-get secrets :user)))
             (dummy (setenv "PASSWORD" (if (functionp secret)
                                           (funcall secret)
                                         secret)))
             (proc (apply 'start-process name name "mbsync" args)))
        (unless quick
          (message (format "mbsync started for channel %s" (car args))))
        (process-put proc :quick quick)
        (process-put proc :channel channel)
        (set-process-filter proc 'vbe:mbsync-filter)
        (set-process-sentinel proc 'vbe:mbsync-sentinel)))))
(setq vbe:mbsync-mode-line-string nil)
(defun vbe:mbsync-update-mode-line (process)
  "Update mode line information about mbsync process"
  (setq vbe:mbsync-mode-line-string
        (let ((status (process-status process)))
          (when (eq status 'run)
            (concat "mbsync:" (process-get process :channel)))))
  (force-mode-line-update))
(defun vbe:mbsync-mode-line ()
  "Display current mbsync mode line if applicable"
  (when (member major-mode '(gnus-group-mode))
    vbe:mbsync-mode-line-string))
(add-to-list 'global-mode-string '(:eval (vbe:mbsync-mode-line)) t)
(defun vbe:mbsync-filter (proc msg)
  (with-current-buffer (process-buffer proc) (comint-truncate-buffer))
  (vbe:mbsync-update-mode-line proc))
(defun vbe:mbsync-sentinel (proc change)
  (vbe:mbsync-update-mode-line proc)
  (when (and (eq (process-status proc) 'exit) (not (process-get proc :quick)))
    (gnus-group-get-new-news 2)))

;; How to trigger mbsync?
(define-key gnus-group-mode-map (kbd "f") 'vbe:mbsync)
(run-with-timer 2 60 'vbe:mbsync "luffy" t) ; quick sync
(run-with-timer (* 5 60) (* 7 60) 'vbe:mbsync "luffy") ; full sync
(run-with-timer 30 (* 3 60) 'vbe:mbsync "exoscale" t)
(run-with-timer (* 11 60) (* 17 60) 'vbe:mbsync "exoscale")

(require 'spam)
(setq spam-install-hooks t)
(spam-initialize)
;; The spam is handled according to groups configuration.
