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
(require 'gnus)
(require 'gnus-msg)
(require 'gnus-identities)
(require 'dired)

;; Do not mangle `message-dont-reply-to-names' on followups.
(ad-disable-advice 'gnus-summary-followup 'before
                   'gnus-identities:gnus-summary-followup)
(ad-activate 'gnus-summary-followup)
;; Posting styles definition
(setq gnus-posting-styles
      `((".*"
         (x-identity "default")
         (name "Vincent Bernat")
         (address ,(s-join "@" '("vincent" "bernat.ch")))
         (signature (vbe:fortune)))
        ((or (vbe:mail-related-to "*@luffy.cx")
             (vbe:mail-related-to "*@luffy.cx" "x-delivered-to"))
         (x-identity "luffy")
         (address ,(s-join "@" '("bernat" "luffy.cx")))
         (signature (vbe:fortune)))
        ((vbe:mail-related-to '("*@debian.org"
                                "*@*.debian.org"
                                "*@debian.ch"
                                "*@*.debian.ch"
                                "*@debconf.org"
                                "*@*.debconf.org"))
         (x-identity "debian")
         (address ,(s-join "@" '("bernat" "debian.org")))
         (organization "Debian")
         (signature (vbe:fortune)))
        ((header "from" "notifications@github.com")
         (signature nil))
        ((header "from" ".*@discoursemail.com")
         (signature nil))
        ((header "subject" "RFS: ")
         (signature (s-join "\n" '("Debian package sponsoring guidelines:"
                                   " https://vincent.bernat.ch/en/debian-package-sponsoring"))))))

(defun vbe:mail-related-to (what &optional fields)
  "Determine if the current message has something to do with WHAT.
It will search in FIELDS (default `To', `Cc' and `From') to check
if any of the given expressions in WHAT is present."
  (when (buffer-live-p gnus-summary-buffer)
    (with-current-buffer gnus-original-article-buffer
      (let ((what (-list what))
            (fields (or (-list fields) '("to" "from" "cc" "newsgroups"))))
        (-any?
         (-not 'null)
         (-map (lambda (field)
                 (when field
                   (-any?
                    (-not 'null)
                    (-map (lambda (address)
                            (-any?
                             (-not 'null)
                             (-map (lambda (w)
                                     (string-match (dired-glob-regexp w) address))
                                   what)))
                          (-non-nil (-map #'second (mail-extract-address-components field t)))))))
               (-map #'message-field-value fields)))))))

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
  (let* ((fortune-buffer (generate-new-buffer " fortune"))
         (fortune-string
          (or
           (unwind-protect
               (with-current-buffer fortune-buffer
                 (apply 'call-process
                        (append (list (or vbe:fortune-program "fortune") nil t nil)
                                vbe:fortune-switches
                                (list (if long-p "-l" "-s"))))
                 (skip-chars-backward "\n\t ")
                 (buffer-substring (point-min) (point))))
           "Have an adequate day.")))
    (kill-buffer fortune-buffer)
    (if (called-interactively-p 'any)
        (insert fortune-string))
    fortune-string))

(provide 'vbe-gnus-identities)
;;; vbe-gnus-identities.el ends here
