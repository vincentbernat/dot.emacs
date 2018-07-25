;;; vbe-package.el --- Package management with package.el and use-package  -*- lexical-binding: t; -*-

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

(require 'package)
(require 'tls)
(setq package-user-dir (concat user-emacs-directory "site-lisp/elpa"))

;; Archive to search for.
(setq package-archives '(("melpa"    . "https://melpa.org/packages/")
                         ("m-stable" . "https://stable.melpa.org/packages/")
                         ("gnu"      . "https://elpa.gnu.org/packages/")))

;; Priorities. Default priority is 0.
(setq package-archive-priorities
      '(("m-stable" . 20)
        ("melpa"    . 10)))

;; Waiting for use-package 2.4...
(setq package-pinned-packages '((use-package . "melpa")))

;; Secure a bit GnuTLS setup. See also:
;;  https://github.com/antifuchs/safe-tls-defaults-mode/
;;
;; These settings are not enough. Instead, use GnuTLS directly.
;; (setq gnutls-min-prime-bits 2048
;;       gnutls-verify-error t
;;       network-security-level 'high)
(setq tls-checktrust t
      tls-program (list
                   (mapconcat
                    'identity
                    '("gnutls-cli -p %p --dh-bits=2048 --ocsp --x509cafile=%t"
                      "--priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:%%PROFILE_MEDIUM'"
                      "%h")
                    " ")))
;; Also disable completely builtin GnuTLS support. Emacs should fallback to tls.el.
(defun vbe:gnutls-available-p (&rest args) nil)
(advice-add 'gnutls-available-p :override 'vbe:gnutls-available-p)

;; Initialize package manager.
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Install use-package and enable it.
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

;; All packages should be installed.
(setq use-package-always-ensure t)

;; Load some additional packages to use :diminish and :bind* options.
(use-package diminish)
(use-package bind-key)

;; Install quelpa
(defvar quelpa-dir)

(use-package quelpa
  :init
  (setq quelpa-dir (concat user-emacs-directory "site-lisp/quelpa")
        quelpa-checkout-melpa-p nil
        quelpa-update-melpa-p nil))
(use-package quelpa-use-package
  :config
  (quelpa-use-package-activate-advice))
(require 'quelpa-use-package)

(provide 'vbe-package)
;;; vbe-package.el ends here
