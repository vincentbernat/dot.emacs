;;; Code:

(require 'org-passwords)
(setq org-passwords-file "~/Documents/org/passwords.gpg"
      org-passwords-random-words-dictionary "/usr/share/dict/american-english"
      org-passwords-time-opened "5 min")

(defun vbe:org-passwords-web-browse ()
  "Browse the URL of the current entry."
  (interactive)
  (save-restriction
    (save-excursion
      (org-narrow-to-subtree)
      (search-backward-regexp "^\\*")
      (search-forward-regexp (concat "^[[:space:]]*:URL:[[:space:]]*"))
      (browse-url (buffer-substring-no-properties (point)
                                                  (funcall (lambda ()
                                                             (end-of-line)
                                                             (point))))))))

(define-key org-passwords-mode-map
  (kbd "C-c u")
  'org-passwords-copy-username)
(define-key org-passwords-mode-map
  (kbd "C-c s")
  'org-passwords-copy-password)
(define-key org-passwords-mode-map
  (kbd "C-c g")
  'vbe:org-passwords-web-browse)

;;; passwords.conf.el ends here
