;;; vbe-orgmode.el --- Configuration for org-mode    -*- lexical-binding: t; -*-

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
(require 'org)
(require 'org-clock)
(require 'org-tempo)

(setq
 org-startup-indented nil               ; don't indent by default
 org-hide-leading-stars t               ; don't show leading stars
 org-cycle-separator-lines 0            ; don't show blank lines between collapsed trees
 org-src-fontify-natively t             ; fontify code blocks
 org-edit-src-content-indentation 0     ; don't indent source blocks
 org-catch-invisible-edits 'error       ; don't edit invisible text
 org-export-use-babel nil               ; do not evaluate blocks when exporting
 org-image-actual-width (when window-system (list (truncate (* (frame-native-width) 0.9))))
 org-export-with-toc nil                ; do not add a ToC when exporting
 org-html-postamble nil                 ; do not add a postamble when exporting
 org-html-head-include-scripts nil      ; do not add Javascript
 org-return-follows-link t              ; follow link directly with return
 org-clock-mode-line-total 'current     ; only display the current clock time in modeline
 org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Make <n create a NOTE block.
(add-to-list 'org-structure-template-alist
             '("n" . "notes"))

;; Ensure code background match current background
(defun vbe:org-inline-css-hook (exporter)
  "Insert custom inline CSS for background color."
  (when (eq exporter 'html)
    (let ((pre-fg (face-foreground 'default))
          (pre-bg (face-background 'default)))
      (setq org-html-head-extra
            (format "<style type=\"text/css\">\n pre.src { background-color: %s; color: %s }</style>\n"
                    pre-bg pre-fg)))))
(add-hook 'org-export-before-processing-hook 'vbe:org-inline-css-hook)

;; Babel modes
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python     . t)
   (shell      . t)                     ; ob-shell replaces ob-sh in Org 9.x
   (sqlite     . t)
   (emacs-lisp . t)))

(use-package ob-async)

;; Git auto-commit
(defvar vbe:git-auto-commit-mode-for-orgmode-directories
  '("~/Documents/org")
  "List of directories where git-auto-commit-mode will be enabled.")
(use-package git-auto-commit-mode
  :hook (org-mode . vbe:git-auto-commit-mode-for-orgmode)
  :config
  (defun vbe:git-auto-commit-mode-for-orgmode ()
    "Enable autocommit mode only for some directories."
    (when (and buffer-file-name
               (--any? (f-ancestor-of? it buffer-file-name)
                       vbe:git-auto-commit-mode-for-orgmode-directories))
      (git-auto-commit-mode 1))))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-pretty-entities t
        org-ellipsis "…")
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p!)" "|" "DONE(d!)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (org-modern-todo-faces
   `(("TODO" :background ,(naquadah-get-colors 'scarlet-red-3) :foreground "white")
     ("NEXT" :background ,(naquadah-get-colors 'sky-blue-3) :foreground "white")
     ("PROGRESS" :background ,(naquadah-get-colors 'chocolate-3) :foreground "white")
     ("DONE" :background ,(naquadah-get-colors 'chameleon-3) :foreground "white")
     ("WAITING" :background ,(naquadah-get-colors 'orange-3) :foreground "white")
     ("HOLD" :background ,(naquadah-get-colors 'plum-3) :foreground "white")
     ("CANCELLED" :background ,(naquadah-get-colors 'aluminium-5) :foreground "white"))))

(use-package org-mime
  :defer t)

(use-package org-download
  :pin "melpa"
  :custom
  (org-download-image-dir "images")
  (org-download-heading-lvl nil))

(use-package htmlize)

(use-package ox-reveal
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@4.1.3"))

(provide 'vbe-orgmode)
;;; vbe-orgmode.el ends here
