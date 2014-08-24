;;; Code:

(require 'org-passwords)
(setq org-passwords-file "~/Documents/org/passwords.gpg"
      org-passwords-random-words-dictionary "/usr/share/dict/american-english"
      org-passwords-time-opened "5 min")

(define-key org-passwords-mode-map
  (kbd "C-c u")
  'org-passwords-copy-username)
(define-key org-passwords-mode-map
  (kbd "C-c p")
  'org-passwords-copy-password)

;;; passwords.conf.el ends here
