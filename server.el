;; Start the server only if we are not root
(unless (string= (user-login-name) "root")
  (require 'server)
  (when (or (not server-process)
            (not (eq (process-status server-process)
                     'listen)))
    (unless (server-running-p server-name)
      (server-start))))

(provide 'vbe/server)
