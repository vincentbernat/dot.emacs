(defun vbe/znc-add-server (server port ssl user networks)
  "Add a server to the list of ZNC servers.

The password will be retrieved with `auth-source-search'."
  (let ((found (nth 0 (auth-source-search :host server
                                          :user user
                                          :require '(:user :secret)
                                          :max 1))))
    (if found
        (let ((user (plist-get found :user))
              (password (let ((secret (plist-get found :secret)))
                          (if (functionp secret)
                              (funcall secret)
                            secret))))
          (add-to-list 'znc-servers
                       (list server
                             port
                             ssl
                             (mapcar (function (lambda (x) (list x user password)))
                                     networks)))))))

(vbe/znc-add-server "znc.luffy.cx" 7667 t "bernat"
                    '(oftc freenode))
