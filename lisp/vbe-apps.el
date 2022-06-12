;;; vbe-apps.el --- Emacs "applications"  -*- lexical-binding: t; -*-

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

(use-package org
  :bind (("C-c l" . org-store-link))
  :pin "gnu"                            ; no effect?
  :config
  (require 'vbe-orgmode))

(use-package pass
  :defer t)

(use-package ein
  :pin "melpa")
(use-package request
  :defer t :ensure nil
  :custom
  (request-storage-directory (vbe:runtime-directory "request")))

(provide 'vbe-apps)
;;; vbe-apps.el ends here
