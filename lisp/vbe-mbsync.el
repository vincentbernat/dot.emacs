;;; vbe-mbsync.el --- mbsync helpers                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Vincent Bernat

;; Author: Vincent Bernat <bernat@luffy.cx>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Small wrapper around mbsync
;;  PassCmd is expected to be `PassCmd "echo ${PASSWORD}"` or if you
;;  want to make it work even when running mbsync from the command
;;  line, use something like this:
;;     PassCmd "echo ${PASSWORD:-$(gpg --no-tty -qd ~/.authinfo.gpg | sed ...)}"

;;; Code:

(require 'vbe-common)
(require 'auth-source)
(require 'comint)

(defun vbe:mbsync (channel &optional only)
  "Run the `mbsync' command asynchronously.

Use the provided CHANNEL. The argument ONLY tells which box
should be synced. It can be a box name, a list of box names or
t for INBOX."
  (interactive (list (completing-read "Channel: "
                                      (vbe:mbsync-channels) nil)
                     current-prefix-arg))
  (let* ((name (format "*mbsync-%s*" channel))
         (args (cond ((stringp only) (format "%s:%s" channel only))
                     ((eq only nil) (format "%s" channel))
                     ((equal only '(4)) (format "%s:INBOX" channel))
                     ((listp only) (format "%s:%s" channel
                                           (s-join "," only)))
                     (t (format "%s:INBOX" channel))))
         (previous (get-process name)))
    (if (and previous (process-live-p previous))
        (when (called-interactively-p 'interactive)
          (error "Another mbsync is already running"))
      (let* ((process-environment (copy-sequence process-environment))
             (secrets (nth 0 (auth-source-search :max 1
                                                 :host (format "mbsync-%s" channel)
                                                 :require '(:user :secret))))
             (secret (plist-get secrets :secret))
             (_ (setenv "USER" (plist-get secrets :user)))
             (_ (setenv "PASSWORD" (if (functionp secret)
                                       (funcall secret)
                                     secret)))
             (proc (apply 'start-process name name "mbsync" (list args))))
        (process-put proc :quick only)
        (process-put proc :channel channel)
        (set-process-filter proc 'vbe:mbsync-filter)
        (set-process-sentinel proc 'vbe:mbsync-sentinel)))))

(defvar vbe:mbsync-mode-line-string nil)
(defvar vbe:mbsync-update-mode-line nil)
(defun vbe:mbsync-update-mode-line (process)
  "Update mode line information about mbsync PROCESS."
  (setq vbe:mbsync-mode-line-string
        (let ((status (process-status process))
              (channel (process-get process :channel)))
          (cond ((eq status 'run)
                 (add-to-list 'vbe:mbsync-update-mode-line channel t))
                ((eq status 'exit)
                 (setq vbe:mbsync-update-mode-line (delete channel vbe:mbsync-update-mode-line))))
          (when vbe:mbsync-update-mode-line
            (let* ((symbols "⠋⠙⠚⠞⠖⠦⠴⠲⠳⠓")
                   (current (or vbe:mbsync-mode-line-string ""))
                   (current-symbol (if (> (length current) 0)
                                       (substring current 0 1)
                                     ""))
                   (next-symbol-index (% (+ 1 (or (s-index-of current-symbol symbols) 0))
                                         (length symbols)))
                   (next-symbol (substring symbols next-symbol-index (+ 1 next-symbol-index))))
              (concat next-symbol " " (s-join " " vbe:mbsync-update-mode-line))))))
  (force-mode-line-update))

(defun vbe:mbsync-mode-line ()
  "Display current mbsync mode line if applicable."
  (when (member major-mode '(gnus-group-mode))
    vbe:mbsync-mode-line-string))

(defun vbe:mbsync-filter (proc msg)
  "Filter for the mbsync process.
Process is PROC and received line is MSG."
  (with-current-buffer (process-buffer proc)
    (comint-truncate-buffer)
    (dolist (msg-line (nbutlast (split-string msg "[\n\r]+")))
      (when (s-present? msg-line)
        (when (buffer-live-p (process-buffer proc))
          (let ((moving (= (point) (process-mark proc))))
            (save-excursion
              (goto-char (process-mark proc))
              (insert (concat (propertize (format-time-string "[%Y-%m-%dT%T%z] ") 'face 'font-lock-doc-face)
                              msg-line
                              "\n"))
              (set-marker (process-mark proc) (point)))
            (if moving (goto-char (process-mark proc))))))))
  (vbe:mbsync-update-mode-line proc))

(defun vbe:mbsync-channels (&optional mbsyncrc)
  "Return existing channels for mbsync.
MBSYNCRC is the configuration file to look at."
  (let ((mbsyncrc (or mbsyncrc (expand-file-name "~/.mbsyncrc"))))
    (-sort (-on '< 'length)
           (-distinct (-map '-third-item
                            (s-match-strings-all "^\\(Channel\\|Group\\)\\s-+\\([a-z0-9A-Z-]+\\)\\s-*$"
                                                 (f-read-bytes mbsyncrc)))))))

(defun vbe:mbsync-sentinel (proc change)
  "Sentinel for mbsync process.
Process is PROC and change is CHANGE."
  (vbe:mbsync-update-mode-line proc)
  (when (and (eq (process-status proc) 'exit) (not (process-get proc :quick)))
    (gnus-group-get-new-news 2)))

(defvar vbe:mbsync-something 0)
(defun vbe:mbsync-something ()
  "Sync something depending on how many time this function has been called."
  (let ((args (cond ((eq (% vbe:mbsync-something 2) 0) '("fastmail" t))
                    ((eq (% vbe:mbsync-something 5) 0) '("fastmail"))
                    nil)))
    (when (and args (vbe:online?))
      (apply 'vbe:mbsync args))
    (setq vbe:mbsync-something (1+ vbe:mbsync-something))))

(provide 'vbe-mbsync)
;;; vbe-mbsync.el ends here
