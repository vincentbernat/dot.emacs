;; ERC configuration

;; Don't track those changes.
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))

;; Enable smileys
(add-to-list 'erc-modules 'smiley)

;; Truncate too long buffers
(add-to-list 'erc-modules 'truncate)
(setq erc-truncate-buffer-on-save t)

;; Use static filling
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 16)

(erc-update-modules)
