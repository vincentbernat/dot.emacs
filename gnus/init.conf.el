(require 'dash)

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
(setq gnus-agent nil)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; How to archive sent messages
(setq gnus-message-archive-group '((cond ((message-news-p) nil)
                                         (t "Sent")))
      gnus-message-archive-method "nnimap:"
      gnus-update-message-archive-method t
      gnus-gcc-mark-as-read t)

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
;;  PassCmd is expected to be `PassCmd "echo ${PASSWORD}"` or if you
;;  want to make it work even when running mbsync from the command
;;  line, use something like this:
;;     PassCmd "echo ${PASSWORD:-$(gpg --no-tty -qd ~/.authinfo.gpg | sed ...)}"
(defun vbe:mbsync (channel &optional only)
  "run the `mbsync` command asynchronously"
  (interactive (list (completing-read "Channel: "
                                 (vbe:mbsync-channels) nil t)
                     current-prefix-arg))
  (let* ((name (format "*mbsync-%s*" channel))
         (args (cond ((stringp only) (format "%s:%s" channel only))
                     ((eq only nil) (format "%s" channel))
                     ((equal only '(4)) (format "%s:INBOX" channel))
                     ((listp only) (format "%s:%s" channel
                                           (mapconcat 'identity only ",")))
                     (t (format "%s:INBOX" channel))))
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
             (proc (apply 'start-process name name "mbsync" (list args))))
        (process-put proc :quick only)
        (process-put proc :channel channel)
        (set-process-filter proc 'vbe:mbsync-filter)
        (set-process-sentinel proc 'vbe:mbsync-sentinel)))))

(setq vbe:mbsync-mode-line-string nil)
(defun vbe:mbsync-update-mode-line (process)
  "Update mode line information about mbsync process"
  (setq vbe:mbsync-mode-line-string
        (let ((status (process-status process)))
          (when (eq status 'run)
            (concat "⇋" (process-get process :channel)))))
  (force-mode-line-update))
(defun vbe:mbsync-mode-line ()
  "Display current mbsync mode line if applicable"
  (when (member major-mode '(gnus-group-mode))
    vbe:mbsync-mode-line-string))

(defun vbe:mbsync-filter (proc msg)
  (with-current-buffer (process-buffer proc)
    (comint-truncate-buffer)
    (dolist (msg-line (nbutlast (split-string msg "[\n\r]+")))
      (when (buffer-live-p (process-buffer proc))
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            (goto-char (process-mark proc))
            (insert (concat (propertize (format-time-string "[%Y-%m-%dT%T%z] ") 'face 'font-lock-doc-face)
                            msg-line
                            "\n"))
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc)))))))
  (vbe:mbsync-update-mode-line proc))

(defun vbe:mbsync-channels (&optional mbsyncrc)
  "Return existing channels for mbsync"
  (let ((mbsyncrc (or mbsyncrc (expand-file-name "~/.mbsyncrc")))
        (result))
    (with-temp-buffer
      (insert-file-contents mbsyncrc)
      (goto-char 1)
      (save-match-data
        (while (re-search-forward "^\\(Channel\\|Group\\)\\s-+\\(\\w+\\)\\s-*$" nil t)
          (setq result (append result (list (match-string 2)))))))
    result))

(defun vbe:mbsync-sentinel (proc change)
  (vbe:mbsync-update-mode-line proc)
  (when (and (eq (process-status proc) 'exit) (not (process-get proc :quick)))
    (gnus-group-get-new-news 2)))

(require 'dbus)
(defvar vbe:mbsync-something 0)
(defun vbe:mbsync-something ()
  "Sync something depending on how many time this function has been called"
  (let ((args (cond ((eq (% vbe:mbsync-something 2) 0) '("luffy" t))
                    ((eq (% vbe:mbsync-something 5) 0) '("luffy"))
                    ((eq (% vbe:mbsync-something 7) 0) '("exoscale-gmail-default" t))
                    ((eq (% vbe:mbsync-something 17) 0) '("exoscale"))
                    nil)))
    (when (and args (vbe:working-network-connection?))
      (apply 'vbe:mbsync args))
    (setq vbe:mbsync-something (1+ vbe:mbsync-something))))

;; How to trigger mbsync?
(define-key gnus-group-mode-map (kbd "f") 'vbe:mbsync)
(cancel-function-timers 'vbe:mbsync-something)
(run-at-time "10 sec" 61 'vbe:mbsync-something)

(require 'spam)
(setq spam-install-hooks t)
(spam-initialize)
;; The spam is handled according to groups configuration.

(require 'gnus-icalendar)
(gnus-icalendar-setup)
