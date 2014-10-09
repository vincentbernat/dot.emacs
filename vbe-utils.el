(defun vbe:run-directory (name)
  "Return a directory for runtime files. Create it if it does not exist."
  (let ((dir (expand-file-name (format "run/%s" name)
			       user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;; The main way to load a file is to put functionalities depending on
;; some other file into a file `somelibrary.conf.el` which will be
;; loaded when `somelibrary` is loaded. The system is a bit smart and
;; if the library has hiphens in its name, it will also search into
;; subdirectories. This feature is inspired from Julien Danjou's emacs
;; configuration.
(defun vbe:after-load (file)
  "Execute appropriate hook after loading FILE.
The hooks are looked in FILE.conf.el in user emacs directory or
in a subdirectory if we can find the appropriate file by
substituting hyphens for slashes."
  ;; We got an absolute filename. Let's find the basename.
  (let* ((filename (file-name-nondirectory file))
	 (name (substring filename 0
			  (string-match "\\.elc?\\>" filename)))
	 (components (split-string name "-"))
	 (directory user-emacs-directory))

    ;; Do we have files for this?
    (unless (string-match "\\." name)
            (dolist (n (reverse (number-sequence 0 (+ (length components)))))
              (let ((target (expand-file-name
                             (format "%s/%s/%s.conf.el"
                                     directory
                                     (mapconcat 'identity (or (butlast components n) '("")) "/")
                                     (mapconcat 'identity (or (last components n) '("init")) "-")))))
                (when (file-readable-p target)
                  (load target)))))))

;; Load current features
(mapc '(lambda (f) (vbe:after-load (symbol-name f))) features)
;; Load future features
(add-hook 'after-load-functions 'vbe:after-load)

(defun vbe:at (where)
  "Return `t' if the current profile is WHERE."
  (string= (cond ((string-match (concat (regexp-quote ".p.fti.net") "$")
			   (system-name))
		  "orange")
                 ((string-match (concat (regexp-quote ".corp.dailymotion.com") "$")
                                (system-name))
                  "dailymotion")
                 ((string-match (concat (regexp-quote ".deezer.com") "$")
                                (system-name))
                  "deezer")
		 (t "unknown"))
	   (cond ((symbolp where) (symbol-name where))
		 ((stringp where) where)
		 (t ""))))

;; http://stackoverflow.com/questions/14201740/replace-region-with-result-of-calling-a-function-on-region
(defun vbe:apply-function-to-region (fn)
  (interactive "aFunction to apply to region: ")
  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (resulting-text
            (funcall
             fn
             (buffer-substring-no-properties beg end))))
      (kill-region beg end)
      (insert resulting-text))))

(defvar vbe:edit-region-mode-map (make-sparse-keymap))
(define-key vbe:edit-region-mode-map "\C-c'" 'vbe:edit-region-exit)
(define-key vbe:edit-region-mode-map "\C-xk" 'vbe:edit-region-exit)
(define-minor-mode vbe:edit-region-mode
  "Minor mode to edit region with another mode")

;; Edit region in another buffer.  This could have been done with just
;; an indirect buffer and narrowing but we want to remove and restore
;; any indentation. For example, this allows one to edit a block
;; literal in YAML. Can specify a mode to use.
(defun vbe:edit-region-in-another-buffer (start end &optional arg)
  (interactive "r\nP")
  (let* ((mode (if arg (intern (completing-read
                                "Mode: "
                                (mapcar (lambda (e) (list (symbol-name e)))
                                        (apropos-internal "-mode$" 'commandp))
                                nil t))
                 nil))
         (wincfg (current-window-configuration))
         (code (buffer-substring-no-properties start end))
         (left-margin (vbe:compute-left-margin code))
         (original (current-buffer))
         (buffer (generate-new-buffer
                  (format "*partial edition of %s*" (buffer-name))))
         ovl margin)

    ;; Setup an overlay
    (setq ovl (make-overlay start end))
    (overlay-put ovl 'face 'secondary-selection)
    (overlay-put ovl :read-only "Leave me alone")

    ;; Create a new buffer with the appropriate content
    (with-current-buffer buffer
      (insert code)
      (indent-rigidly (point-min) (point-max) (* left-margin -1))
      (if mode
          (funcall mode)
        (set-auto-mode))
      (goto-char (point-min))

      ;; Set some variables to restore situation
      (setq-local vbe:edit-region--start start)
      (setq-local vbe:edit-region--end end)
      (setq-local vbe:edit-region--ovl ovl)
      (setq-local vbe:edit-region--original original)
      (setq-local vbe:edit-region--left-margin left-margin)
      (setq-local vbe:edit-region--wincfg wincfg)

      ;; Remove file name to not allow saving
      (setq buffer-file-name nil)

      ;; Set minor mode
      (vbe:edit-region-mode))
    (pop-to-buffer buffer)
    (message (substitute-command-keys
              "Edit, then exit with \\[vbe:edit-region-exit]"))))
(global-set-key "\C-c'" 'vbe:edit-region-in-another-buffer)

;; Compute left margin of a region
(defun vbe:compute-left-margin (code)
  (require 's)
  (require 'dash)
  (-min
   (-map '(lambda (line) (length (car (s-match "^\\s-*" line))))
         (-remove 's-blank? (s-lines code)))))

;; Exit editing region in buffer
(defun vbe:edit-region-exit ()
  (interactive)
  (let ((start vbe:edit-region--start)
        (end vbe:edit-region--end)
        (ovl vbe:edit-region--ovl)
        (original vbe:edit-region--original)
        (left-margin vbe:edit-region--left-margin)
        (wincfg vbe:edit-region--wincfg)
        code)
    (indent-rigidly (point-min) (point-max) left-margin)
    (setq code (buffer-substring-no-properties (point-min) (point-max)))
    (with-current-buffer original
      (save-excursion
        (delete-overlay ovl)
        (delete-region start end)
        (goto-char start)
        (insert code)))
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    (switch-to-buffer original)
    (when wincfg
      (set-window-configuration wincfg))))

(provide 'vbe/utils)
