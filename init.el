;;; init.el --- Emacs init file                      -*- lexical-binding: t; -*-

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

;(package-initialize)

;; Setup environment to load other files.

(setq load-prefer-newer t)
(setq user-emacs-directory (file-name-directory
                        (or (buffer-file-name) (file-chase-links load-file-name))))
(setq custom-file (concat user-emacs-directory "custom.el"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'vbe-package)
(require 'vbe-looks)
(require 'vbe-ergonomics)
(require 'vbe-programming)
(require 'vbe-server)

(use-package gnus
  :commands (gnus)
  :config
  (require 'vbe-gnus))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :pin "gnu"                            ; no effect?
  :config
  (require 'vbe-orgmode))

(use-package erc
  :commands (znc-erc znc-all erc)
  :config
  (require 'vbe-erc))

;;; init.el ends here
