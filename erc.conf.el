;; ERC configuration

;; Don't track those changes.
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

;; Enable smileys
(add-to-list 'erc-modules 'smiley)
(erc-update-modules)

