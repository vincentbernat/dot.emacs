;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Org-related packages
(package! org-download)

;; Major modes
(package! debian-el)
(package! dpkg-dev-el)
(package! dockerfile-mode)
(package! lua-mode)
(package! protobuf-mode)
(package! puppet-mode)
(package! junos-mode :recipe (:host github
                              :repo "vincentbernat/junos-mode"
                              :files (:defaults "junos.py")))
(package! tide :disable t)
(package! anaconda-mode :disable t)

;; Misc packages
(package! apheleia)
(package! bm)
(package! git-auto-commit-mode)
(package! openbsd-knf-style :recipe (:host github
                                     :repo "vincentbernat/openbsd-knf-emacs"))
