;; ERC configuration

(require 'erc)
(require 'erc-fill)
(require 'erc-track)
(require 'dash)
(require 's)

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
 ;; Special encoding for some channels
 erc-server-coding-system '(utf-8 . utf-8)
 erc-encoding-coding-alist '(("#gcu" . iso-8859-15)))

(setq
 ;; Switch to the most active buffer first
 erc-track-switch-direction 'mostactive)
;; But put very important messages first
(add-hook 'erc-track-list-changed-hook 'erc-track-sort-by-importance)

;; A query buffer is "urgent"
(defadvice erc-track-find-face (around vbe:erc-track-find-face-promote-query activate)
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
                                      (create-image (expand-file-name "icons/debian.png" user-emacs-directory))
                                      '(:ascent center)))
                      str))
                    str)))
  "Substitutions to use when shortening channel names. Notably, a
  common prefix can be substitued with a logo.")

(defvar vbe:erc-track-prefix-network
  `(("exonet" ,(let ((str "❬❱"))
                 (when (display-images-p)
                   (add-text-properties
                    0 2
                    (list 'display (append
                                    (create-image (expand-file-name "icons/exoscale.png" user-emacs-directory))
                                    '(:ascent center)))
                    str))
                 str)))
  "Prefix the channel with the given string (after #) if the
  channel is part of the given network. This is useful if some
  network is using too generic names.")

(defun vbe:erc-track-transform-names (channel-names)
  "Transform channel names with bells and whistles. Some suffixes
  are changed to some Unicode chars. Additionally, for some
  servers, we may add an additional prefix."
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
  "Shorten channel names even more than
`erc-track-shorten-names'.  Some suffix are changed to some
Unicode chars. Additionally, for some servers, we may add an
additional suffix."
  (erc-unique-channel-names
   (vbe:erc-track-transform-names (erc-all-buffer-names))
   (vbe:erc-track-transform-names channel-names)
   (lambda (s)
     (> (length s) erc-track-shorten-cutoff))
   erc-track-shorten-start))

;; Enable smileys
(add-to-list 'erc-modules 'smiley)

;; Truncate too long buffers
(add-to-list 'erc-modules 'truncate)
(setq erc-truncate-buffer-on-save t)

;; Use static filling
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-column 90)
(setq erc-fill-static-center 24)

;; Enable keep-place module to not move the point
(add-to-list 'erc-modules 'keep-place)

(defun vbe:erc-reset-track-mode ()
  "Reset channel tracking."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))

(defun vbe:erc-clean-closed ()
  "Kill any buffer whose server is not alive"
  (interactive)
  (erc-buffer-list
   (lambda ()
     (when (not (erc-server-process-alive))
       (kill-buffer)))))

(defun vbe:erc-quit-all ()
  "Close any open connection"
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
  "Extract timestamp beginning a message and display it like a regular timestamp.

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
  `(let ((,red   (car ,color))
         (,green (car (cdr ,color)))
         (,blue  (car (cdr (cdr ,color)))))
     ,@body))

(defun vbe:rgb-to-html (color)
  (vbe:unpack-color color red green blue
   (concat "#" (format "%02x%02x%02x" red green blue))))

(defun vbe:hexcolor-luminance (color)
  (vbe:unpack-color color red green blue
   (floor (+ (* 0.299 red) (* 0.587 green) (* 0.114 blue)))))

(defun vbe:invert-color (color)
  (vbe:unpack-color color red green blue
   `(,(- 255 red) ,(- 255 green) ,(- 255 blue))))

(defun vbe:erc-get-color-for-nick (nick dark)
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
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "<\\([^>]*\\)>")
	(let ((nick (match-string 1)))
	  (put-text-property (match-beginning 1) (match-end 1) 'face
			     (cons 'foreground-color
				   (vbe:erc-get-color-for-nick nick 't)))))))

(add-hook 'erc-insert-modify-hook 'erc-fill) ; Order is important
(add-hook 'erc-insert-modify-hook 'vbe:erc-put-color-on-nick)
(add-hook 'erc-mode-hook 'emojify-mode)

;; Too easy to hit C-c C-c instead of C-c C-space
(define-key erc-mode-map "\C-c\C-c" nil)
