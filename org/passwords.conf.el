;;; Code:

(require 's)
(require 'org-passwords)
(setq org-passwords-file "~/Documents/org/passwords.gpg"
      org-passwords-random-words-dictionary "/usr/share/dict/american-english"
      org-passwords-time-opened "2 min")

(define-key org-passwords-mode-map
  (kbd "C-c d u")
  'org-passwords-copy-username)
(define-key org-passwords-mode-map
  (kbd "C-c d p")
  'org-passwords-copy-password)
(define-key org-passwords-mode-map
  (kbd "C-c d g")
  'org-passwords-open-url)
(define-key org-passwords-mode-map
  (kbd "C-c d o")
  'vbe:org-passwords-copy-otp)

(defun vbe:org-passwords-copy-otp ()
  "Execute the OTP program for the current entry and copy its
result in the kill-ring buffer"
  (interactive)
  (kill-new (s-trim (shell-command-to-string
                     (org-passwords-get-property "OTP")))))

;;; passwords.conf.el ends here
