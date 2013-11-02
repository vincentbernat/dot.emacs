(defun vbe/znc-add-server (server port user networks)
  "Add a server to the list of ZNC servers.

We use SSL inconditionaly. Moreover, we don't store the password
but put nil instead. At least, we tweak the username to contain
the network name later, this will be separated again."
  (add-to-list 'znc-servers
               (list server
                     port
                     t                  ; SSL enabled
                     (mapcar (function (lambda (slug) (list slug
                                                            (format "%s/%s" user slug)
                                                            nil)))
                                     networks))))

(defun vbe/znc-erc-ssl-connector (&rest R)
  "Connect to ERC using SSL and retrieve password with `auth-source-search'.

Moreover, handle multiple networks by sending the password with
the appropriate network slug that we extract from the nick."
  (let* ((user (nth 0 (split-string (plist-get R :nick) "/")))
         (slug (nth 1 (split-string (plist-get R :nick) "/")))
         (found (nth 0 (auth-source-search :host (plist-get R :server)
                                           :user user
                                           :require '(:user :secret)
                                           :max 1))))
    (if found
        (let ((password (let ((secret (plist-get found :secret)))
                          (if (functionp secret)
                              (funcall secret)
                            secret))))
          (plist-put R :password (format "%s/%s:%s" user slug password))
          (plist-put R :nick user)
          (apply 'erc-tls R)))))
(setq znc-erc-ssl-connector 'vbe/znc-erc-ssl-connector)

;; Define networks
(vbe/znc-add-server "znc.luffy.cx" 7667 "bernat"
                    '(oftc freenode))
