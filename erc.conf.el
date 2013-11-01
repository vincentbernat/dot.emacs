;; ERC configuration

;;;###autoload
(defun znc ()
  """Connect to ZNC bouncer"""
  (interactive)
  (erc-tls :server "znc.luffy.cx"
           :port 7667
           :nick "bernat/oftc")
  (erc-tls :server "znc.luffy.cx"
           :port 7667
           :nick "bernat/freenode"))

;; Don't track those changes.
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
