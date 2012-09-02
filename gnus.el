;; Do not store files in News
(setq gnus-directory (vbe:run-directory "gnus")
      gnus-home-directory gnus-directory)

(provide 'vbe:gnus)
