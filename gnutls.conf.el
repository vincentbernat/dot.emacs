; Don't use an insecure connection by default!
(defun open-gnutls-stream (name buffer host service)
  (gnutls-negotiate :process (open-network-stream name buffer host service)
                    :hostname host
                    :verify-hostname-error t :verify-error t))
