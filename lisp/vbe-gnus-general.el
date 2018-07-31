;;; vbe-gnus-general.el --- General Gnus configuration  -*- lexical-binding: t; -*-

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

;;; Code:

;;; General setup

(require 'vbe-common)

(require 'gnus)
(require 'gnus-start)
(require 'gnus-msg)
(require 'gnus-art)
(require 'mml2015)
(require 'auth-source)
(require 'epg)
(require 'nnir)

;; No .newsrc
(setq gnus-directory (vbe:runtime-directory "gnus")
      gnus-home-directory gnus-directory
      gnus-read-newsrc-file nil
      gnus-save-newsrc-file nil
      gnus-startup-file (f-join gnus-home-directory "newsrc"))

;; Temporary directories
(setq
 message-directory (vbe:runtime-directory "gnus" "mail")
 message-auto-save-directory (vbe:runtime-directory "gnus" "mail" "drafts"))

;; Servers to use
(setq gnus-select-method
      `(nnimap ""
               (nnimap-stream shell)
               (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:~/.mbsync/mails/luffy")
               (nnir-search-engine imap)))
(setq gnus-secondary-select-methods
      `((nnimap "exoscale"
                (nnimap-stream shell)
                (nnimap-shell-program "/usr/lib/dovecot/imap -o mail_location=maildir:~/.mbsync/mails/exoscale")
                (nnir-search-engine imap))
        (nndraft ""
                 (nndraft-directory ,(nnheader-concat message-directory "drafts")))))
(setq gnus-agent nil)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq gnus-message-archive-group "Sent"
      gnus-message-archive-method "nnimap:"
      gnus-update-message-archive-method t
      gnus-gcc-mark-as-read t)

;; Sync IMAP with mbsync
(require 'vbe-mbsync)
(cancel-function-timers 'vbe:mbsync-something)
(run-at-time "10 sec" 61 'vbe:mbsync-something)

;; Spam handling
(require 'spam)
(setq spam-install-hooks t)
(spam-initialize)

;; Hide too long citations
(setq gnus-treat-hide-citation t
      gnus-treat-highlight-citation t
      gnus-article-skip-boring t
      gnus-cited-lines-visible '(3 . 6))

;; Buttons
(setq gnus-inhibit-mime-unbuttonizing nil ; Display some buttons
      gnus-buttonized-mime-types '("multipart/alternative"
                                   "multipart/signed"
                                   "multipart/encrypted")
      mm-discouraged-alternatives '("text/html"
                                    "text/richtext"
                                    "multipart/related"))

;; Visible headers
(setq gnus-visible-headers
      (mapcar #'(lambda (header) (format "^%s:" header))
              (-flatten (-map 'split-string '("From Organization Subject Newsgroups"
                                              "To Cc Reply-To Followup-To Mail-Followup-To"
                                              "Thread-Topic"
                                              "X-Mailer X-Newsreader"
                                              "User-Agent X-Posting-Agent"
                                              "X-Spam-Level" "Date")))))

;; Gravatar
(require 'gnus-gravatar)
(defun vbe:gnus-treat-from-gravatar ()
  "Display gravatar, only when online."
  (when (vbe:online?)
    (gnus-treat-from-gravatar)))
(add-hook 'gnus-article-prepare-hook #'vbe:gnus-treat-from-gravatar)

;; Citation format
(setq message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format " ❦ %e %B %Y %R %Z, %f:\n")

;; Increase score of followups
(setq gnus-kill-files-directory (vbe:runtime-directory "gnus" "scores"))
(add-hook 'message-sent-hook #'gnus-score-followup-thread)

;; Prefer 8-bit encoding
(add-to-list 'mm-content-transfer-encoding-defaults '("text/plain" 8bit))

;; Sign messages
(setq mml2015-use 'epg           ; use epg
      mm-verify-option 'never    ; never check for sigs (for SMIME, this takes too long)
      mm-decrypt-option 'always  ; always decrypt
      auth-source-gpg-encrypt-to user-mail-address)

(setq gnus-signature-limit 12.0)        ; No more than 12 lines for a signature

;; Set user name and email address
(defvar vbe:mail-addresses)
(setq user-full-name "Vincent Bernat"
      user-mail-address "bernat@luffy.cx")
(setq vbe:mail-addresses
      (--map (format "\\(^\\|[^.]\\)\\b%s[@\\.]" it)
             (apply 'append (-map 'split-string
                                  '("bernat vbernat vincent.bernat"
                                    "vbe Vincent.Bernat"))))
      ;; When to display To: instead of From:
      gnus-ignored-from-addresses vbe:mail-addresses
      ;; Addresses to prune on wide reply
      message-dont-reply-to-names (append vbe:mail-addresses
                                          (-map 'regexp-quote
                                                '("@noreply.github.com"
                                                  "notifications@github.com"
                                                  "control@bugs.debian.org"
                                                  "submit@bugs.debian.org"))))

(setq message-valid-fqdn-regexp
      (format "\\(%s\\|.*\\.digital\\|.*\\.group\\)" message-valid-fqdn-regexp))

;; Use the list of subscribed addresses for MFT from the group/topic parameters
(setq message-subscribed-address-functions
      '(gnus-find-subscribed-addresses))
(setq gnus-parameters
      '(("^OS\\.Debian\\."
         (subscribed . t))))

;; Ability to reply on top (shame, shame)...
(defun vbe:gnus/wide-reply-on-top (n)
  "Wide reply on top of the current message.
Uses the process/prefix convention with N."
  (interactive "P")
  (vbe:gnus/reply-somehow-on-top n 'gnus-summary-wide-reply-with-original))
(defun vbe:gnus/reply-on-top (n)
  "Reply on top of the current message.
Uses the process/prefix convention with N."
  (interactive "P")
  (vbe:gnus/reply-somehow-on-top n 'gnus-summary-reply-with-original))

(defun vbe:gnus/reply-somehow-on-top (n how)
  "Reply using N and HOW on top of the current message.
Uses the process/prefix convention with N."
  (cl-flet ((escape (s) (if (and s (string-match "%" s))
                            (mapconcat (lambda (c)
                                         (if (eq c ?%)
                                             "%%"
                                           (char-to-string c)))
                                       s "")
                          s)))
    (let ((message-cite-reply-position 'above)
          (message-citation-line-format
           (s-join "\n" (delq nil
                              `(" ――――――― Original Message ―――――――"
                                " From: %f"
                                " Sent: %e %B %Y %R %Z"
                                ,(escape (concat " Subject: " (gnus-with-article-headers
                                                               (mail-fetch-field "Subject"))))
                                ,(escape (vbe:gnus/extract-names "To"))
                                ,(escape (vbe:gnus/extract-names "Cc"))
                                "")))))
    (funcall how n))))

(defun vbe:gnus/extract-names (field)
  "Extract the list of names from a given FIELD."
  (let ((result (gnus-with-article-headers
            (mapconcat #'(lambda (x) (or (first x)
                                    (second x)))
                       (mail-extract-address-components
                        (or (mail-fetch-field field)
                            "") t) "; "))))
    (when (> (length result) 0)
      (concat " " field ": "
              (with-temp-buffer
                (insert result)
                (fill-region (point-min) (point-max))
                (replace-string "\n" (concat "\n"
                                             (make-string (+ 3 (length field))
                                                          ? ))
                                nil (point-min) (point-max))
                (buffer-substring (point-min) (point-max)))))))

(bind-keys :map gnus-group-mode-map
           ("f" . vbe:mbsync)
           :map gnus-summary-mode-map
           ("f" . vbe:gnus/wide-reply-on-top)
           ("r" . vbe:gnus/reply-on-top))

(provide 'vbe-gnus-general)
;;; vbe-gnus-general.el ends here
