;;; vbe-looks.el --- Fashion and aesthetics     -*- lexical-binding: t; -*-

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

;; TODO: https://github.com/cpitclaudel/monospacifier

(defvar vbe:default-font "Hack-11"
  "Default font.")
(defvar vbe:modeline-font "Iosevka-11"
  "Font to use for the modeline (and minibuffer prompt).")

;; Disable various graphical widgets
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode blink-cursor-mode))
    (when (fboundp mode) (funcall mode -1)))

;; Various minor modes
(show-paren-mode 1)			; Display matching parenthesis (C-M-n and C-M-p).
(line-number-mode 1)			; Show line number.
(column-number-mode 1)			; Show column number.
(global-hl-line-mode 1)			; Highlight current line.

(setq inhibit-splash-screen t)		; No splash screen.
(setq make-pointer-invisible t)		; Hide the mouse while typing.
(setq x-stretch-cursor t)               ; Stretch cursor to match character width.
(set-default 'indicate-buffer-boundaries '((up . nil) (down . nil) (t . left)))

(defun vbe:set-font (&optional frame)
    "Change default font for the given FRAME.
If not frame is provided, the font is applied to all frames and
future frames."
    (when window-system
      (set-face-attribute 'default frame :font vbe:default-font)
      (dolist (face '(mode-line
                      mode-line-inactive
                      minibuffer-prompt))
        (set-face-attribute face frame :font vbe:modeline-font))))

;; Fringe
(defun vbe:fringe-mode ()
  "Fix fringe width, depending on DPI."
  (fringe-mode (frame-char-width)))
(vbe:fringe-mode)
(add-hook 'vbe:dpi-change-hook #'vbe:fringe-mode)

;; Main theme.
(use-package naquadah-theme
  :config
  (vbe:set-font (selected-frame))
  (add-hook 'after-make-frame-functions #'vbe:set-font)
  (naquadah-theme-set-faces
   'naquadah
   '(gnus-summary-selected      (:background chameleon-3 :foreground black))
   '(gnus-summary-normal-ticked (:background scarlet-red-3))
   '(gnus-summary-normal-read   (:foreground aluminium-3 :italic t))
   '(gnus-summary-high-unread   (:inherit gnus-summary-normal-unread :background sky-blue-2))
   '(gnus-summary-high-read     (:inherit gnus-summary-normal-read :background sky-blue-3))

   '(org-tag                    (:background aluminium-5
                                 :foreground "white"
                                 :box (:line-width 1 :color aluminium-3)
                                 :slant oblique
                                 :weight normal
                                 :height 0.8))

   '(comint-highlight-prompt    (:foreground orange-2 :weight bold))

   '(highlight                  (:background scarlet-red-3))
   '(hl-line                    (:background aluminium-6))))

;; Modeline theme.
(use-package spaceline
  :pin "melpa"
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'zigzag
        powerline-display-buffer-size t
        powerline-display-hud nil
        spaceline-hud-p nil
        spaceline-buffer-encoding-abbrev-p nil)

  (spaceline-define-segment vbe:projectile-root
    "Show the current projectile root and nothing if no root is detected."
    (when (fboundp 'projectile-project-name)
      (let ((project-name (projectile-project-name)))
        (unless (or (string= project-name "-")
                    (string= project-name (buffer-name))
                    (not (buffer-file-name)))
          project-name))))

  (spaceline-define-segment vbe:version-control
    "Show version vc-mode, without any flag."
    (when vc-mode
      (powerline-raw (s-trim
                      (replace-regexp-in-string "^ Git." " " vc-mode)))))

  (spaceline-compile
    ;; Left
    `(((buffer-modified
        buffer-size)
       :face highlight-face
       :priority 100)
      ((buffer-id remote-host)
       :priority 98)
      (((major-mode :priority 89)
        (process :when active :priority 50)))
      ((flycheck-error flycheck-warning flycheck-info)
       :when active :priority 79)
      (minor-modes :when active :priority 9)
      (erc-track :when active :priority 80)
      (vbe:projectile-root :priority 60)
      (vbe:version-control :when active :priority 68)
      (org-clock :when active :priority 75))
    ;; Right
    `((global :when active)
      ((point-position
        line-column)
       :priority 96)
      (buffer-position :priority 99)))
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

  ;; Handle DPI change.
  (defun vbe:powerline-reset ()
    (setq powerline-height (round (* 1.3 (frame-char-height))))
    (pl/reset-cache))
  (vbe:powerline-reset)
  (add-hook 'vbe:dpi-change-hook #'vbe:powerline-reset)

  ;; Don't change the modeline on focus out.
  (remove-hook 'focus-out-hook 'powerline-unset-selected-window))

;; Display emojis.
(use-package emojify
  :defer t
  :config
  (setq emojify-emojis-dir (vbe:runtime-directory "emojis")))

;; Display ^L as break lines
(use-package page-break-lines
  :diminish
  :config
  (global-page-break-lines-mode 1))

(provide 'vbe-looks)
;;; vbe-looks.el ends here
