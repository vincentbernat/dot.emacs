;; ERC configuration

(require 'erc)
(require 'erc-fill)
(require 'erc-track)

(setq
 ;; Don't track those changes.
 erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                           "324" "329" "332" "333" "353" "477")
 ;; Don't reconnect automatically
 erc-server-auto-reconnect nil
 ;; When opening a new buffer, bury it
 erc-join-buffer 'bury
 ;; When someone query me, bury it too
 erc-auto-query 'bury
 ;; Special encoding for some channels
 erc-server-coding-system '(utf-8 . utf-8)
 erc-encoding-coding-alist '(("#gcu" . iso-8859-15)))

;; A query buffer is "urgent"
(defadvice erc-track-find-face (around vbe:erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p)
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

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
    (while (re-search-forward "\\w+" nil t)
      (let* ((bounds (bounds-of-thing-at-point 'word))
             (nick   (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (when (erc-get-server-user nick)
          (put-text-property
           (car bounds) (cdr bounds) 'face
           (cons 'foreground-color (vbe:erc-get-color-for-nick nick 't))))))))

(add-hook 'erc-insert-modify-hook 'vbe:erc-put-color-on-nick)
(add-hook 'erc-mode-hook (lambda ()
                           (modify-syntax-entry ?\_ "w" nil)
                           (modify-syntax-entry ?\- "w" nil)))

(erc-update-modules)
