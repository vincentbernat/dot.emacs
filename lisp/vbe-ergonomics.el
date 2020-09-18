;;; vbe-ergonomics.el --- General ergonomics and behaviours  -*- lexical-binding: t; -*-

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

(require 'vbe-package)

;; Various settings
(setq mouse-yank-at-point t         ; Yank where the point currently is
      mouse-1-click-follows-link nil  ; Don't follow links with left click
      x-select-enable-primary t     ; Yank use the primary selection if available
      x-select-enable-clipboard t   ; Yank use the clipboard if available
      save-interprogram-paste-before-kill t ; Put clipboard/selection into kill ring
      echo-keystrokes 0.1           ; Show keystrokes early
      use-dialog-box nil              ; Do not display dialog boxes.
      sentence-end-double-space nil   ; No need to end sentences with two spaces.
      next-screen-context-lines 5)  ; Keep more lines when scrolling.
(setq-default indent-tabs-mode nil)       ; Don't use tabs by default
(fset 'yes-or-no-p 'y-or-n-p)           ; Always use y/n prompt

;; Auto-compile non-compiled files
(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

;; After a pause, describe the current map.
(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

;; Setup ivy/swiper/counsel
(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
         ("C-SPC" . ivy-restrict-to-matches))
  :demand t
  :custom
  (ivy-use-virtual-buffers nil)
  (ivy-count-format "(%d/%d) ")
  (ivy-extra-directories nil)
  (ivy-format-functions-alist '((t . vbe:ivy-format-function-arrow)))
  :config
  (defun vbe:ivy-format-function-arrow (cands)
    "Transform CANDS into a string for minibuffer with an unicode arrow prefix."
    (ivy--format-function-generic
     (lambda (str)
       (concat "▶ " (ivy--add-face str 'ivy-current-match)))
     (lambda (str)
       (concat "  " str))
     cands
     "\n"))
  (ivy-mode 1))
(use-package ivy-posframe
  :after ivy
  :diminish
  :hook
  (ivy-mode . ivy-posframe-mode)
  :custom-face
  (ivy-posframe ((t (:background "#333244"))))
  (ivy-posframe-border ((t (:background "#333244"))))
  :custom
  (ivy-posframe-parameters '((internal-border-width . 12)))
  (ivy-posframe-height-alist '((swiper . 15)
                               (find-file . 20)
                               (t      . 25)))
  (ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-window-center)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode 1))
(use-package swiper
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward))
  :config
  (add-to-list 'swiper-font-lock-exclude 'sieve-mode))
(use-package smex
  :custom
  (smex-save-file (vbe:runtime-file "smex-items")))
(use-package counsel
  :diminish
  :config
  (counsel-mode 1))

;; Setup projectile
(use-package projectile
  :diminish
  :bind-keymap (("C-c p" . projectile-command-map))
  :custom
  (projectile-known-projects-file (vbe:runtime-file "projectile" "bookmarks.eld"))
  (projectile-cache-file (vbe:runtime-file "projectile" "cache"))
  (projectile-completion-system 'ivy)
  (projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode 1))

;; Edit indirect allows to edit a region into a separate buffer
(use-package edit-indirect
  :bind (("C-c '" . edit-indirect-region))
  :config
  (defvar vbe:edit-indirect--left-margin 0)
  (defun vbe:compute-left-margin (code)
    "Compute left margin of a string of CODE."
    (-min
     (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
           (-remove 's-blank? (s-lines code)))))
  (defun vbe:after-indirect-edit-remove-left-margin ()
    "Remove left-margin and save it into a local variable."
    (let ((lm (vbe:compute-left-margin (buffer-substring (point-min) (point-max)))))
    (indent-rigidly (point-min) (point-max) (* -1 lm))
    (setq-local vbe:edit-indirect--left-margin lm)))
  (defun vbe:after-indirect-edit-restore-left-margin ()
    "Restore left-margin before commiting."
    (indent-rigidly (point-min) (point-max) vbe:edit-indirect--left-margin))
  (add-hook 'edit-indirect-after-creation-hook #'vbe:after-indirect-edit-remove-left-margin)
  (add-hook 'edit-indirect-before-commit-hook #'vbe:after-indirect-edit-restore-left-margin))

;; Multiple cursors.
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e"   . mc/edit-ends-of-lines)
         ("C-S-c C-a"   . mc/edit-beginnings-of-lines)
         ("C-c SPC"     . set-rectangular-region-anchor))
  :pin "melpa"
  :custom
  (mc/list-file (vbe:runtime-file "mc-list.el")))

;; Fast cursor moves
(use-package avy
  :bind (:map goto-map
         ("j"   . avy-goto-subword-1)
         ("M-j" . avy-goto-subword-1)))

;; Bookmarks
(use-package bm
  :bind (("C-c C-." . bm-toggle)
         ("C-c C-/" . bm-next)
         ("C-c C-," . bm-previous))
  :custom
  (bm-cycle-all-buffers t))

;; Restore last-known position when opening a file.
(use-package saveplace
  :custom
  (save-place-file (vbe:runtime-file "places"))
  :config
  (save-place-mode 1))

;; Make buffer names unique
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/")
  (uniquify-after-kill-buffer-p t "rename after killing")
  (uniquify-ignore-buffers-re "^\\*"))

;; ibuffer
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

;; Vertical alignment
(use-package align
  :bind (("C-c |" . align-current)))

;; Hippie expansion
(use-package hippie-exp
  :bind (("M-/" . hippie-expand)))

;; Auto revert buffers
(global-auto-revert-mode 1)
(setq
 ;; Don't create lock files
 create-lockfiles nil
 ;; Don't create backup files
 make-backup-files nil)
;; Auto saves in a specific directory
(let ((tmp (vbe:runtime-file "saves" "saves-")))
  (setq auto-save-list-file-prefix tmp
        auto-save-file-name-transforms `((".*" ,tmp t))))

;; How to quit Emacs
(setq confirm-kill-emacs #'y-or-n-p
      confirm-kill-processes nil)

;; Disable Insert as on my X1 this key is mapped near other modifiers
;; and can be hit by accident.
(global-unset-key (kbd "<insert>"))

;; See: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun vbe:smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace/symbol character on this
line.  If point is already there, move to the beginning of the
line.  Effectively toggle between the first non-whitespace/symbol
character and the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    ;; Expanded from (back-to-indentation) but also skip symbols
    (beginning-of-line 1)
    (skip-syntax-forward " _" (line-end-position))
    (backward-prefix-chars)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'vbe:smarter-move-beginning-of-line)

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Miscellaneous tweaks for packages
(use-package transient
  :ensure nil :defer t :custom
  (transient-levels-file (vbe:runtime-file "transient" "levels.el"))
  (transient-values-file (vbe:runtime-file "transient" "values.el"))
  (transient-history-file (vbe:runtime-file "transient" "history.el")))

;; Miscellaneous tweaks for non-packages
(use-package tramp
  :ensure nil :defer t :config
  (setq tramp-persistency-file-name (vbe:runtime-file "tramp" "history")))

(use-package pcache
  :ensure nil :defer t :config
  (setq pcache-directory (vbe:runtime-directory "pcache")))

(use-package url
  :ensure nil :defer t :custom
  (url-configuration-directory (vbe:runtime-directory "url")))

(use-package url-cache
  :ensure nil :defer t :custom
  (url-cache-directory (vbe:runtime-directory "url")))

(use-package vc-hooks
  :ensure nil :defer t :custom
  (vc-follow-symlinks t))


(provide 'vbe-ergonomics)
;;; vbe-ergonomics.el ends here
