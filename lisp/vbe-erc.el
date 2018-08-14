;;; vbe-erc.el --- ERC configuration                 -*- lexical-binding: t; -*-

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
(require 'erc)
(require 'erc-fill)
(require 'erc-track)
(require 'erc-log)

(setq
 ;; Don't track those changes.
 erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                           "324" "329" "332" "333" "353" "477")
 ;; Track changes in server buffers
 erc-track-exclude-server-buffer nil
 ;; Don't track KGB-* (in #debian-* channels mostly)
 erc-track-exclude '("KGB-0" "KGB-1" "KGB-2" "KGB-3")
 ;; Don't reconnect automatically
 erc-server-auto-reconnect nil
 ;; When opening a new buffer, bury it
 erc-join-buffer 'bury
 ;; When someone query me, bury it too
 erc-auto-query 'bury
 ;; Kill buffers for server messages after quitting the server
 erc-kill-server-buffer-on-quit t
 ;; Do not enable logging
 erc-log-channels-directory nil
 ;; Special encoding for some channels
 erc-server-coding-system '(utf-8 . utf-8)
 erc-encoding-coding-alist '(("#gcu" . iso-8859-15)))

(setq
 ;; Switch to the most active buffer first
 erc-track-switch-direction 'mostactive)
;; But put very important messages first
(add-hook 'erc-track-list-changed-hook #'erc-track-sort-by-importance)

(defadvice erc-track-find-face (around vbe:erc-track-find-face-promote-query activate)
  "Make query buffer \"urgent\"."
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

;; Shorten names in mode-line
(setq
 erc-track-shorten-cutoff 4        ; shorten from 4 characters or more
 erc-track-shorten-function 'vbe:erc-track-shorten-names)

(defvar vbe:erc-track-substitutions
  `(("#debian-" ,(let ((str "#@"))
                   (when (display-images-p)
                     (add-text-properties
                      1 2
                      (list 'display (append
                                      (create-image (f-join user-emacs-directory "icons" "debian.png"))
                                      '(:ascent center)))
                      str))
                    str)))
  "Substitutions to use when shortening channel names.
Notably, a common prefix can be substitued with a logo.")

(defvar vbe:erc-track-prefix-network
  `(("exonet" ,(let ((str "❬❱"))
                 (when (display-images-p)
                   (add-text-properties
                    0 2
                    (list 'display (append
                                    (create-image (f-join user-emacs-directory "icons" "exoscale.png"))
                                    '(:ascent center)))
                    str))
                 str)))
  "Prefix the channel with the given string.
The prefix is added after # if the channel is part of the given
network. This is useful if some network is using too generic
names.")

(defun vbe:erc-track-transform-names (channel-names)
  "Transform CHANNEL-NAMES with bells and whistles.
Some suffixes are changed to some Unicode chars. Additionally,
for some servers, we may add an additional prefix."
  (-map
   (lambda (l) (-if-let
                   (subst (-first
                           (lambda (subst) (s-starts-with? (car subst) l)) vbe:erc-track-substitutions))
                   (s-prepend (nth 1 subst) (s-chop-prefix (car subst) l))
                 l))
   (-map
    (lambda (l) (-if-let
                    (prefix (and (s-starts-with? "#" l)
                                 (get-buffer l)
                                 (with-current-buffer l
                                   (-first (lambda (pfx) (equal (and (fboundp 'erc-network-name) (erc-network-name))
                                                                (car pfx))) vbe:erc-track-prefix-network))))
                    (s-prepend (concat "#" (nth 1 prefix)) (s-chop-prefix "#" l))
                  l))
    channel-names)))

(defun vbe:erc-track-shorten-names (channel-names)
  "Shorten CHANNEL-NAMES even more than `erc-track-shorten-names'.
Some suffix are changed to some Unicode chars. Additionally, for
some servers, we may add an additional suffix."
  (erc-unique-channel-names
   (vbe:erc-track-transform-names (erc-all-buffer-names))
   (vbe:erc-track-transform-names channel-names)
   (lambda (s)
     (> (length s) erc-track-shorten-cutoff))
   erc-track-shorten-start))

(defadvice erc-modified-channels-object (after vbe:erc-modified-channels-object activate)
  "Shorten even more the modified-channels object.
It needs to not be too long for it to be displayed in the
modeline. Otherwise, spaceline would just hide it."
  (setq ad-return-value
        (let ((s ad-return-value)
              (len 20)
              (ellipsis "…"))
          (if (> (length s) len)
              (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
            s))))

;; Truncate too long buffers
(add-to-list 'erc-modules 'truncate)
(setq erc-truncate-buffer-on-save t)

;; Define filling
(defun vbe:erc-fill ()
  "Fills a text such that messages start at column `erc-fill-static-center'.
And until value of `frame-width'. Also, alter `erc-fill-column'
to match this value to enable the timestamp to be correctly
positioned."
  (save-match-data
    (goto-char (point-min))
    (looking-at "^\\(\\S-+\\)")
    (let ((nick (match-string 1))
          (min-width (+ erc-fill-static-center 20))
          (max-width 120))
      (let ((fill-column (- (min max-width (max min-width
                                                (- (frame-width) 10)))
                            (erc-timestamp-offset)))
            (fill-prefix (make-string erc-fill-static-center 32)))
        (setq erc-fill-column fill-column)
        (insert (make-string (max 0 (- erc-fill-static-center
                                       (length nick) 1))
                             32))
        (erc-fill-regarding-timestamp))
      (erc-restore-text-properties))))
(setq erc-fill-function 'vbe:erc-fill)
(setq erc-fill-static-center 24)

;; Enable keep-place module to not move the point
(add-to-list 'erc-modules 'keep-place)

(defun vbe:erc-reset-track-mode ()
  "Reset channel tracking."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(defun vbe:erc-clean-closed ()
  "Kill any buffer whose server is not alive."
  (interactive)
  (erc-buffer-list
   (lambda ()
     (when (not (erc-server-process-alive))
       (kill-buffer)))))

(defun vbe:erc-quit-all ()
  "Close any open connection."
  (interactive)
  (erc-buffer-list
   (lambda ()
     (when (erc-server-process-alive)
       (erc-quit-server "Gone"))))
  (run-at-time 4 nil 'vbe:erc-clean-closed))

;; ZNC will replay a buffer and prefix each message with a
;; timestamp. Let's extract this timestamp and redefine current-time
;; to make them appear as regular timestamp. We use an advice to be
;; able to locally define `current-time` function.
(defadvice erc-display-line-1 (around vbe:erc-display-line-1 activate)
  "Turn ZNC timestamps into regular timestamps.
For this advice to work, the timestamp should be `[TTxxxxxxx]'
where `xxxxxxx' is the number of seconds since epoch."
  (save-match-data
    (let ((orig-string (ad-get-arg 0)))
      (if (string-match "^\\(?:\\s-*\\*\\)?\\s-*\\S-+ \\[TT\\([0-9]+\\)\\] " orig-string)
          (let ((seconds (string-to-number (substring orig-string
                                                      (match-beginning 1)
                                                      (match-end 1))))
                (start (- (match-beginning 1) 3))
                (end (+ (match-end 1) 2)))
            (ad-set-arg 0 (concat (substring orig-string 0 start)
                                  (substring orig-string end)))
            (let (orig-current-time)
              (fset 'orig-current-time (symbol-function 'current-time))
              (fset 'current-time (lambda ()
                                    (list (lsh seconds -16)
                                          (logand seconds (- (lsh 1 16) 1))
                                          0
                                          0)))
              (unwind-protect
                  ad-do-it
                (fset 'current-time (symbol-function 'orig-current-time)))))
        ad-do-it))))


;; Use different colors for nick. This is mainly stolen from:
;;   http://www.emacswiki.org/emacs/ErcNickColors

(defmacro vbe:unpack-color (color red green blue &rest body)
  "Create color COLOR from RED, GREEN and BLUE.
Created color is bound to COLOR. Then, execute BODY."
  `(let ((,red   (car ,color))
         (,green (car (cdr ,color)))
         (,blue  (car (cdr (cdr ,color)))))
     ,@body))

(defun vbe:rgb-to-html (color)
  "Convert COLOR to HTML notation."
  (vbe:unpack-color color red green blue
   (concat "#" (format "%02x%02x%02x" red green blue))))

(defun vbe:hexcolor-luminance (color)
  "Extract luminance from COLOR."
  (vbe:unpack-color color red green blue
   (floor (+ (* 0.299 red) (* 0.587 green) (* 0.114 blue)))))

(defun vbe:invert-color (color)
  "Invert provided COLOR."
  (vbe:unpack-color color red green blue
   `(,(- 255 red) ,(- 255 green) ,(- 255 blue))))

(defun vbe:erc-get-color-for-nick (nick dark)
  "Compute a color for provided NICK.
DARK tells if we have a dark theme or not."
  (let* ((hash     (md5 (downcase nick)))
         (red      (mod (string-to-number (substring hash 0 10) 16) 256))
         (blue     (mod (string-to-number (substring hash 10 20) 16) 256))
         (green    (mod (string-to-number (substring hash 20 30) 16) 256))
         (color    `(,red ,green ,blue)))
    (vbe:rgb-to-html (if (if dark (< (vbe:hexcolor-luminance color) 85)
                       (> (vbe:hexcolor-luminance color) 170))
                     (vbe:invert-color color)
                   color))))

(defun vbe:erc-put-color-on-nick ()
  "Colorize a nick."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "<\\([^>]*\\)>")
	(let ((nick (match-string 1)))
	  (put-text-property (match-beginning 1) (match-end 1) 'face
			     (cons 'foreground-color
				   (vbe:erc-get-color-for-nick nick 't)))))))

(add-hook 'erc-insert-modify-hook #'erc-fill) ; Order is important
(add-hook 'erc-insert-modify-hook #'vbe:erc-put-color-on-nick)
(add-hook 'erc-mode-hook #'emojify-mode)

;; Too easy to hit C-c C-c instead of C-c C-space
(bind-keys :map erc-mode-map
           ("C-c C-c" . nil))

;; Also configure ZNC
(use-package znc
  :config
  (require 'auth-source)
  (defun vbe:znc-add-server (server port user networks)
    "Add a server to the list of ZNC servers.

We use SSL inconditionaly. Moreover, we don't store the password
but put nil instead. At least, we tweak the username to contain
the network name later, this will be separated again."
    (add-to-list 'znc-servers
                 (list server
                       port
                       t                ; SSL enabled
                       (mapcar (function (lambda (slug) (list slug
                                                         (format "%s/%s" user slug)
                                                         nil)))
                               networks))))

  (defun vbe:znc-erc-ssl-connector (&rest R)
    "Connect to ERC using SSL and retrieve password with `auth-source-search'.

Moreover, handle multiple networks by sending the password with
the appropriate network slug that we extract from the nick."
    (let* ((user (nth 0 (split-string (plist-get R :nick) "/")))
           (slug (nth 1 (split-string (plist-get R :nick) "/")))
           (found (nth 0 (auth-source-search :host (plist-get R :server)
                                             :port (plist-get R :port)
                                             :user user
                                             :require '(:user :secret)
                                             :max 1))))
      (if found
          (let ((password (let ((secret (plist-get found :secret)))
                            (if (functionp secret) (funcall secret)
                              secret))))
            (plist-put R :password (format "%s/%s:%s" user slug password))
            (plist-put R :nick user)
            (apply 'erc-tls R)))))
  (setq znc-erc-ssl-connector 'vbe:znc-erc-ssl-connector)

  ;; Redefine prompt function to not use deprecated default initial-input.
  (defun vbe:znc-prompt-string-or-nil (prompt &optional completions default require-match)
    (let* ((string (completing-read (concat prompt ": ") completions nil require-match nil nil default))
           (string (if (equal string "") nil string)))
      string))
  (advice-add #'znc-prompt-string-or-nil :override #'vbe:znc-prompt-string-or-nil)

  ;; Define networks
  (vbe:znc-add-server "znc.luffy.cx" 7667 "bernat"
                      '(exoscale oftc freenode)))

(provide 'vbe-erc)
;;; vbe-erc.el ends here
