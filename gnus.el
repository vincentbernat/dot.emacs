(defun vbe/gnus-load ()
  "Load Gnus"
  (message "[vbe/] Loading gnus...")
  (vbe/require 'gnus-theme)		; theme for Gnus
  (vbe/require 'gnus-signature)		; how to compute the signature
  (vbe/require 'gnus-identity)		; user identity
  (vbe/require 'gnus-servers)		; mail servers
  (vbe/require 'gnus-article)		; how to format articles
  (vbe/require 'gnus-summary)		; how to format the summary
  (vbe/require 'gnus-bbdb)		; bbdb
  (vbe/require 'gnus-composition)	; message composition
  (vbe/require 'gnus-spam)		; spam handling

  (vbe/sync-packages)
)

;; Install gnus package
(vbe/add-package (list :name "nognus"
		       :init '(add-hook 'gnus-before-startup-hook 'vbe/gnus-load)))

(provide 'vbe/gnus)
