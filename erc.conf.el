;; ERC configuration

(require 'erc)
(require 'erc-fill)
(require 'erc-track)

(setq
 ;; Don't track those changes.
 erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                           "324" "329" "332" "333" "353" "477")
 ;; Switch to the newest buffer first (instead of oldest)
 erc-track-switch-direction 'newest
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
    (if (looking-at "<\\([^>]*\\)>")
	(let ((nick (match-string 1)))
	  (put-text-property (match-beginning 1) (match-end 1) 'face
			     (cons 'foreground-color
				   (vbe:erc-get-color-for-nick nick 't)))))))

(add-hook 'erc-insert-modify-hook 'erc-fill) ; Order is important
(add-hook 'erc-insert-modify-hook 'vbe:erc-put-color-on-nick)

;; Too easy to hit C-c C-c instead of C-c C-space
(define-key erc-mode-map "\C-c\C-c" nil)

;; Fix some bugs in erc as shipped in Emacs 24.4.1.
;;  http://lists.gnu.org/archive/html/erc-discuss/2014-10/msg00003.html
(when (version= emacs-version "24.4.1")
  (defun erc-channel-end-receiving-names ()
    (when erc-channel-new-member-names
      (maphash (lambda (nick user)
                 (if (null (gethash nick erc-channel-new-member-names))
                     (erc-remove-channel-user nick)))
               erc-channel-users)
      (setq erc-channel-new-member-names nil)))
  (defun erc-channel-receive-names (names-string)
    (let (prefix op-ch voice-ch names name op voice)
      (setq prefix (erc-parse-prefix))
      (setq op-ch (cdr (assq ?o prefix))
            voice-ch (cdr (assq ?v prefix)))
      ;; We need to delete "" because in XEmacs, (split-string "a ")
      ;; returns ("a" "").
      (setq names (delete "" (split-string names-string)))
      (let ((erc-channel-members-changed-hook nil))
        (dolist (item names)
          (let ((updatep t))
            (if (rassq (elt item 0) prefix)
                (cond ((= (length item) 1)
                       (setq updatep nil))
                      ((eq (elt item 0) op-ch)
                       (setq name (substring item 1)
                             op 'on
                             voice 'off))
                      ((eq (elt item 0) voice-ch)
                       (setq name (substring item 1)
                             op 'off
                             voice 'on))
                      (t (setq name (substring item 1)
                               op 'off
                               voice 'off)))
              (setq name item
                    op 'off
                    voice 'off))
            (when updatep
              (unless erc-channel-new-member-names
                (erc-channel-begin-receiving-names))
              (puthash (erc-downcase name) t
                       erc-channel-new-member-names)
              (erc-update-current-channel-member
               name name t op voice)))))
      (run-hooks 'erc-channel-members-changed-hook))))

(erc-update-modules)
