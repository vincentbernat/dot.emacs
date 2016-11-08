;; My Debian address
(setq debian-changelog-mailing-address "bernat@debian.org")

;; Add UNRELEASE at the front place
(add-to-list 'debian-changelog-allowed-distributions "UNRELEASED" nil
             (lambda (a b) nil))

;; See https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=803767
(advice-add 'debian-changelog-date-string :around
            (lambda (f)
              (let ((system-time-locale "C"))
                (funcall f))))
