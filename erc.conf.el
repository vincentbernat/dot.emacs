;; ERC configuration

;; Don't track those changes.
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

;; Don't reconnect automatically
(setq erc-server-auto-reconnect nil)

;; Enable smileys
(add-to-list 'erc-modules 'smiley)

;; Truncate too long buffers
(add-to-list 'erc-modules 'truncate)
(setq erc-truncate-buffer-on-save t)

;; Use static filling
(setq erc-fill-function 'erc-fill-static)
(make-variable-buffer-local 'erc-fill-column)
(make-variable-buffer-local 'erc-fill-static-center)
(defun vbe/erc-configure-fill ()
  (save-excursion
    (walk-windows
     (lambda (w)
       (let ((buffer (window-buffer w)))
         (set-buffer buffer)
         (when (eq major-mode 'erc-mode)
           (let* ((width (max 50 (min (window-width w) 100)))
                  (nick-width (max (/ width 4) 15)))
             (setq erc-fill-column (- width 4)
                   erc-fill-static-center nick-width))))))))
(add-hook 'window-configuration-change-hook 'vbe/erc-configure-fill)

;; Enable keep-place module to not move the point
(add-to-list 'erc-modules 'keep-place)

(erc-update-modules)
