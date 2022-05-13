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

;; Tune GC for faster startup
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
          gc-cons-percentage 0.1)))

;; Setup environment to load other files.

(setq warning-minimum-level :error)
(setq load-prefer-newer t)
(setq user-emacs-directory (file-name-directory
                        (or (buffer-file-name) (file-chase-links load-file-name))))
(setq custom-file (concat user-emacs-directory "custom.el"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'vbe-package)
(require 'vbe-looks)
(require 'vbe-ergonomics)
(require 'vbe-programming)
(require 'vbe-apps)
(require 'vbe-server)

;;; init.el ends here
