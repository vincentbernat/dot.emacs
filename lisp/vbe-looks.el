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

(defvar vbe:default-font "DejaVu Sans Mono-11"
  "Default font.")
(defvar vbe:modeline-font "DejaVu Sans-10"
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
    "Change default font for the given FRAME."
    (when window-system
      (when frame
        (select-frame frame))
      (set-face-attribute 'default nil :font vbe:default-font)
      (dolist (face '(mode-line
                      mode-line-inactive
                      minibuffer-prompt))
        (set-face-attribute face nil :font vbe:modeline-font))))

;; Main theme.
(use-package naquadah-theme
  :config
  (vbe:set-font)
  (add-hook 'after-make-frame-functions 'vbe:set-font)
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
   '(hl-line                    (:background aluminium-6))))

;; Modeline theme.
(use-package spaceline
  :pin "m-stable"
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'zigzag
        powerline-display-buffer-size t
        powerline-display-hud nil
        spaceline-hud-p nil
        spaceline-buffer-encoding-abbrev-p nil)
  (spaceline-emacs-theme)

  ;; Handle DPI change.
  (defun vbe:powerline-reset ()
    (setq powerline-height (round (* 1.3 (frame-char-height))))
    (pl/reset-cache))
  (vbe:powerline-reset)
  (add-hook 'vbe:dpi-change-hook 'vbe:powerline-reset)

  ;; Don't change the modeline on focus out.
  (remove-hook 'focus-out-hook 'powerline-unset-selected-window))

;; Display emojis.
(use-package emojify
  :defer t
  :config
  (setq emojify-emojis-dir (vbe:runtime-directory "emojis")))

(provide 'vbe-looks)
;;; vbe-looks.el ends here
