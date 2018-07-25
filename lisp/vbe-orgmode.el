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

(setq
 org-startup-indented t                 ; indent by default
 org-tags-column -70                    ; align tags on the 70th columns
 org-hide-leading-stars t               ; don't show leading stars
 org-cycle-separator-lines 0            ; don't show blank lines between collapsed trees
 org-src-fontify-natively t             ; fontify code blocks
 org-edit-src-content-indentation 0     ; don't indent source blocks
 org-catch-invisible-edits 'error       ; don't edit invisible text
 org-pretty-entities t                  ; use "pretty entities"
 org-clock-mode-line-total 'current     ; only display the current clock time in modeline
 org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Make <n create a NOTE block.
(add-to-list 'org-structure-template-alist
             '("n" "#+BEGIN_NOTES\n?\n#+END_NOTES"))

;; Todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROGRESS(p!)" "|" "DONE(d!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
      org-todo-keyword-faces
      `(("TODO"
         :box (:line-width 1 :color ,(naquadah-get-colors 'scarlet-red-1))
         :background ,(naquadah-get-colors 'scarlet-red-3)
         :foreground "white"
         :weight bold)
        ("NEXT"
         :box (:line-width 1 :color ,(naquadah-get-colors 'sky-blue-1))
         :background ,(naquadah-get-colors 'sky-blue-3)
         :foreground "white"
         :weight bold)
        ("PROGRESS"
         :box (:line-width 1 :color ,(naquadah-get-colors 'chocolate-1))
         :background ,(naquadah-get-colors 'chocolate-3)
         :foreground "white"
         :weight bold)
        ("DONE"
         :box (:line-width 1 :color ,(naquadah-get-colors 'chameleon-1))
         :background ,(naquadah-get-colors 'chameleon-3)
         :foreground "white"
         :weight bold)
        ("WAITING"
         :box (:line-width 1 :color ,(naquadah-get-colors 'orange-1))
         :background ,(naquadah-get-colors 'orange-3)
         :foreground "white"
         :weight bold)
        ("HOLD"
         :box (:line-width 1 :color ,(naquadah-get-colors 'plum-1))
         :background ,(naquadah-get-colors 'plum-3)
         :foreground "white"
         :weight bold)
        ("CANCELLED"
         :box (:line-width 1 :color ,(naquadah-get-colors 'aluminium-3))
         :background ,(naquadah-get-colors 'aluminium-5)
         :foreground "white"
         :weight bold)))

;; Git auto-commit
(defvar vbe:git-auto-commit-mode-for-orgmode-directories
  '("~/Documents/org")
  "List of directories where git-auto-commit-mode will be enabled.")
(use-package git-auto-commit-mode
  :hook (org-mode . vbe:git-auto-commit-mode-for-orgmode)
  :config
  (defun vbe:git-auto-commit-mode-for-orgmode ()
    "Enable autocommit mode only for some directories."
    (when (--any? (f-ancestor-of? it buffer-file-name)
                  vbe:git-auto-commit-mode-for-orgmode-directories)
      (git-auto-commit-mode 1))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" )))

(use-package org-mime)

(use-package org-passwords
  ;; Use my own fork.
  :quelpa (org-passwords :fetcher git
                         :url "https://bitbucket.org/vbernat/org-passwords.el.git"
                         :branch "feature/clear-clipboard")
  :commands (org-passwords)
  :config
  (setq org-passwords-file "~/Documents/org/passwords.gpg"
        org-passwords-random-words-dictionary "/usr/share/dict/american-english"
        org-passwords-time-opened "2 min")
  (define-key org-passwords-mode-map
    (kbd "C-c d u")
    'org-passwords-copy-username)
  (define-key org-passwords-mode-map
    (kbd "C-c d p")
    'org-passwords-copy-password)
  (define-key org-passwords-mode-map
    (kbd "C-c d g")
    'org-passwords-open-url)
  (define-key org-passwords-mode-map
    (kbd "C-c d o")
    'vbe:org-passwords-copy-otp)

  (defun vbe:org-passwords-copy-otp ()
    "Copy OTP password for the current entry.
Execute the OTP program for the current entry and copy its result
in the kill-ring buffer"
    (interactive)
    (kill-new (s-trim (shell-command-to-string
                       (org-entry-get (point) "OTP"))))))

(provide 'vbe-orgmode)
;;; vbe-orgmode.el ends here
