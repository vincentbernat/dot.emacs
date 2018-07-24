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
(mapc #'(lambda (f) (vbe:after-load (symbol-name f))) features)
;; Load future features
(add-hook 'after-load-functions 'vbe:after-load)

(defun vbe:working-network-connection? ()
  "Check we have a working connection. Also check that it is
cheap enough."
  (let ((ofNM "org.freedesktop.NetworkManager"))
    (and (member ofNM
                 (dbus-list-names :system))
         (let ((primary-connection
                (dbus-get-property :system
                                   ofNM "/org/freedesktop/NetworkManager"
                                   ofNM "PrimaryConnection")))
           ;; We need a primary connection
           (and primary-connection
                ;; Is it a full connection ?
                (eq (dbus-get-property :system
                                       ofNM "/org/freedesktop/NetworkManager"
                                       ofNM "State")
                    70)                 ; network manager state is NM_STATE_CONNECTED_GLOBAL
                ;; Does it involve a modem connection?
                (--none? (eq (dbus-get-property :system
                                                ofNM it
                                                (concat ofNM ".Device") "DeviceType")
                             8)   ; 8 = NM_DEVICE_TYPE_MODEM
                         (dbus-get-property :system
                                            ofNM primary-connection
                                            (concat ofNM ".Connection.Active") "Devices")))))))

(provide 'vbe/utils)
