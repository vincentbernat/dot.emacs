(require 'epg)
(setq mml2015-use 'epg		 ; use epg
      mm-verify-option 'always	 ; always check for sigs
      mm-decrypt-option 'always  ; always decrypt
      auth-source-gpg-encrypt-to user-mail-address)

(provide 'vbe:gnus/gpg)
