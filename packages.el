;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Org-related packages
(package! org-download)

;; Major modes
(package! debian-el)
(package! dockerfile-mode)
(package! dpkg-dev-el)
(package! protobuf-mode)
(package! puppet-mode)
(package! junos-mode :recipe (:host github :repo "vincentbernat/junos-mode"))

;; Misc packages
(package! bm)
(package! git-auto-commit-mode)
(package! openbsd-knf-style :recipe (:host github :repo "hogand/openbsd-knf-emacs"))
