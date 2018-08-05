;;; vbe-gnus-looks.el --- Gnus looks  -*- lexical-binding: t; -*-

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
(require 'cl)
(require 'gnus-sum)
(require 'gnus-group)
(require 'gnus-topic)
(require 'gnus-art)

(defun gnus-user-format-function-@ (header)
  "Display @ for message with attachment in summary line.
You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'.
Take a HEADER as argument."
  (let ((case-fold-search t)
        (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                   "text/plain"))
        (indicator " "))
    (when (string-match "^multipart/mixed" ctype)
      (setq indicator "@"))
    indicator))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yest, %H:%M")
        (604800 . "%a %H:%M") ;;that's one week
        ((gnus-seconds-month) . "%a %d")
        ((gnus-seconds-year) . "%b %d")
        ((* 30 (gnus-seconds-year)) . "%b %d '%y")
        (t . "")))

;; Change some fonts
(defface vbe:gnus-summary-subject-face
  `((((type graphic))
     :family "DejaVu Sans"
     :background ,(face-attribute 'default :background)))
  "Subject font in Gnus summary"
  :group 'gnus-sum)
(defface vbe:gnus-summary-@-face
  '((t :inherit font-lock-warning-face))
  "Font for @ (attachments) sign in Gnus summary"
  :group 'gnus-sum)
(defface vbe:gnus-summary-symbols-face
  '((t :inherit shadow))
  "Font for symbols in Gnus summary"
  :group 'gnus-sum)
(defface vbe:gnus-summary-threads-face
  `((t
     :inherit shadow
     :background ,(face-attribute 'default :background)))
  "Font for thread symbols in Gnus summary"
  :group 'gnus-sum)
(defface vbe:gnus-summary-mouse-face
  '((t))                                ; Remove highlighting
  "Font for mouse highlight in Gnus summary"
  :group 'gnus-sum)
(defvar gnus-face-9  'vbe:gnus-summary-@-face)
(defvar gnus-face-10 'vbe:gnus-summary-symbols-face)
(defvar gnus-face-11 'vbe:gnus-summary-subject-face)
(defvar gnus-face-12 'vbe:gnus-summary-threads-face)
(defvar gnus-mouse-face-13 'vbe:gnus-summary-mouse-face)
(setq gnus-summary-line-format
      (concat
       "%10{%U%R%z%}" "%13(" " " "%1{%11,11&user-date;%}"
       "%10{│%}"
       "%9{%u&@;%}" "%-15,15f " "%)"
       "%*"
       " " "%12{%B%}"
       "%11{%s%}\n"))

;; Disable hl-line-mode for Gnus summary
(make-variable-buffer-local 'global-hl-line-mode)
(add-hook 'gnus-summary-mode-hook (lambda () (setq global-hl-line-mode nil)))

(setq
 gnus-summary-to-prefix "→ "
 gnus-sum-thread-tree-single-indent   "◎ "
 gnus-sum-thread-tree-false-root      "◌ "
 gnus-sum-thread-tree-root            "┌ "
 gnus-sum-thread-tree-vertical        "│"
 gnus-sum-thread-tree-leaf-with-other "├─►"
 gnus-sum-thread-tree-single-leaf     "╰─►"
 gnus-sum-thread-tree-indent          "  "
 gnus-summary-newsgroup-prefix "⇋"
 ;; Marks
 gnus-ticked-mark ?⚑
 gnus-dormant-mark ?⚐
 gnus-expirable-mark ?◌
 gnus-read-mark ?✓
 gnus-del-mark ?✗
 gnus-killed-mark ?⚡
 gnus-replied-mark ?↩
 gnus-forwarded-mark ?⇥
 gnus-cached-mark ?∞
 gnus-recent-mark ?★
 gnus-unseen-mark ?⊚
 gnus-unread-mark ?●
 gnus-score-over-mark ?↑           ; ↑ ☀
 gnus-score-below-mark ?↓)         ; ↓ ☂
;; Reevaluate gnus-auto-expirable-marks with those new symbols
(setq gnus-auto-expirable-marks (eval (car (get 'gnus-auto-expirable-marks 'standard-value))))

;; Group line format. Mostly stolen from Julien Danjou
(setq gnus-group-line-format "%ue%uM %S%p %P%5y:%B%(%g%)%O\n"
      gnus-topic-line-format "%i— %(%{%n%}%) %v\n"
      gnus-topic-display-empty-topics nil)

(defun gnus-user-format-function-e (_)
  "Format function for e."
  (vbe:gnus-image-or-space (char-to-string gnus-unread-mark)
                           (f-join user-emacs-directory "icons" "email.png")
                           (> (string-to-number gnus-tmp-number-of-unread) 0)))
(defun gnus-user-format-function-M (_)
  "Formation function for M."
  (vbe:gnus-image-or-space (char-to-string gnus-ticked-mark)
                           (f-join user-emacs-directory "icons" "important.png")
                          (cdr (assq 'tick gnus-tmp-marked))))

(defun vbe:gnus-image-or-space (string image image-p)
  "Insert an image or a space.
Propertize STRING by appending an IMAGE. If IMAGE-P is nil, put a
space instead."
  (let ((image (create-image image)))
    (if (display-images-p)
        (if image-p
            (propertize string 'display
                        (append image
                                '(:ascent center)))
          (propertize " " 'display `(space . (:width ,(car (image-size image))))))
      (if image-p string " "))))


(setq nnmail-extra-headers
      '(To Cc Newsgroups Content-Type Thread-Topic Thread-Index))

(setq gnus-thread-hide-subtree nil      ; expand threads
      gnus-summary-make-false-root 'empty ; add an empty node when needing a root node
      gnus-summary-make-false-root-always nil ; but only if needed
      gnus-fetch-old-headers nil
      gnus-build-sparse-threads 'some   ; fetch some messages to get better threads
      gnus-single-article-buffer t)     ; no more than one buffer per article

;; Better visibility of HTML emails
(setq shr-color-visible-distance-min 10
      shr-color-visible-luminance-min 60)

; Start in topic mode
(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

;; Use emojify for emojis
(add-hook 'gnus-article-mode-hook #'emojify-mode)
(add-hook 'message-mode-hook #'emojify-mode)

(provide 'vbe-gnus-looks)
;;; vbe-gnus-looks.el ends here
