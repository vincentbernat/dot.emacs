;; Debian-related modes
(vbe/add-package '(:name dpkg-dev-el))

(setq debian-changelog-mailing-address "bernat@debian.org") ; Debian address

(provide 'vbe/debian)
