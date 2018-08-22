;;; vbe-tls.el --- SSL tuning for Emacs  -*- lexical-binding: t; -*-

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

(require 'tls)

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

(provide 'vbe-tls)
;;; vbe-tls.el ends here
