; Default theme for Emacs is OK but the one for Gnus...

(defun vbe/gnus-custom-faces ()
  "Custom faces for Gnus"
  (custom-set-faces
   '(gnus-summary-selected-face ((t (:background "DarkOliveGreen1" :underline nil))))
   '(gnus-signature-face ((t (:foreground "orange red"))))
   '(gnus-header-content-face ((t (:font "DejaVu Sans" :foreground "RoyalBlue4"
					 :weight normal :italic nil :slant normal))))
   '(gnus-header-from-face ((t (:font "DejaVu Sans"))))
   '(gnus-header-subject-face ((t (:font "DejaVu Sans" :foreground "chartreuse4"
					 :weight bold))))
   '(gnus-header-name-face ((t (:font "DejaVu Sans" :foreground "chartreuse4"
				      :italic nil :slant normal))))
   '(gnus-header-newsgroup-face ((t (:font "DejaVu Sans"))))))

(add-hook 'gnus-started-hook
	  'vbe/gnus-custom-faces)

(provide 'vbe/gnus-theme)
