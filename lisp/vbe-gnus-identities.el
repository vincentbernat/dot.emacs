;;; vbe-gnus-identities.el --- Gnus identities configuration  -*- lexical-binding: t; -*-

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

(require 'vbe-common)
(require 'gnus-identities)

;; Do not mangle `message-dont-reply-to-names' on followups.
(ad-disable-advice 'gnus-summary-followup 'before
                   'gnus-identities:gnus-summary-followup)
(ad-activate 'gnus-summary-followup)
;; Posting styles definition
(setq gnus-posting-styles
      '((".*"
         (x-identity "default")
         (name "Vincent Bernat")
         (address "bernat@luffy.cx")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@bernat.im"))
         (x-identity "bernat.im")
         (address "vincent@bernat.im")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@crans.org"
                                "*@*.crans.org"
                                "*@crans.ens-cachan.fr"
                                "crans.*" "tac.*"
                                "*@ens-cachan.fr"))
         (x-identity "crans")
         (address "bernat@crans.org")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@debian.org"
                                "*@*.debian.org"
                                "*@debian.ch"
                                "*@*.debian.ch"
                                "*@debconf.org"
                                "*@*.debconf.org"))
         (x-identity "debian")
         (eval (vbe:gnus/will-sign-message))
         (address "bernat@debian.org")
         (organization "Debian")
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@exoscale.net"
                                "*@exoscale.com"
                                "*@exoscale.ch"))
         (x-identity "exoscale")
         (address "Vincent.Bernat@exoscale.com")
         (organization "exoscale")
         ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587 vbe@exoscale.ch")
         (gcc nil)
         (signature (s-join "\n" '("Vincent Bernat — Vincent.Bernat@exoscale.com"
                                   "❬❱ https://www.exoscale.com"))))
        ((header "subject" "RFS: ")
         (signature (s-join "\n" '("Debian package sponsoring guidelines:"
                                   " https://vincent.bernat.im/en/debian-package-sponsoring"))))))

(require 'dired)
(defun vbe:mail-related-to (what &optional fields)
  "Determine if the current message has something to do with WHAT.
It will search in FIELDS (default `To', `Cc' and `From') to check
if any of the given expressions in WHAT is present."
  (when (buffer-live-p gnus-summary-buffer)
    (gnus-with-article-buffer
      (let ((what (if (listp what) what (list what)))
            (matched nil)
            (fields (if fields fields '("to" "from" "cc" "newsgroups"))))
        (dolist (field (mapcar 'message-fetch-field fields))
          (when field
            (dolist (address (delq nil (mapcar 'second
                                               (mail-extract-address-components
                                                field t))))
              (dolist (w what)
                (message (dired-glob-regexp w))
                (when (string-match (dired-glob-regexp w) address)
                  (push address matched))))))
        (when matched t)))))

(defun vbe:gnus/will-sign-message ()
  "Setup a local hook to make the article signed."
  (add-hook 'gnus-message-setup-hook
            'mml-secure-message-sign-pgpmime t t))

;; Signature
(defconst vbe:fortune-program nil
  "*Program used to generate epigrams, default \"fortune\".")

(defvar vbe:fortune-switches (list "-e"
                                   "50%" (expand-file-name "~/.sigs/literature")
                                   "50%" (expand-file-name "~/.sigs/prog-style"))
  "*List of extra arguments when `vbe:fortune-program' is invoked.")

(defun vbe:fortune (&optional long-p)
  "Generate a random epigram.
An optional prefix LONG-P argument generates a long epigram.
The epigram is inserted at point if called interactively."
  (interactive "*P")
  (let ((fortune-buffer (generate-new-buffer " fortune"))
        (fortune-string "Have an adequate day."))
    (unwind-protect
        (save-excursion
          (set-buffer fortune-buffer)
          (apply 'call-process
                 (append (list (or vbe:fortune-program "fortune") nil t nil)
                         vbe:fortune-switches
                         (list (if long-p "-l" "-s"))))
          (skip-chars-backward "\n\t ")
          (setq fortune-string (buffer-substring (point-min) (point))))
      (kill-buffer fortune-buffer))
    (if (interactive-p)
        (insert fortune-string))
    fortune-string))

(provide 'vbe-gnus-identities)
;;; vbe-gnus-identities.el ends here
