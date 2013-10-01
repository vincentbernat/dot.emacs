;; My Debian address
(setq debian-changelog-mailing-address "bernat@debian.org")

;; Add UNRELEASE at the front place
(add-to-list 'debian-changelog-allowed-distributions "UNRELEASED" nil
             (lambda (a b) nil))
