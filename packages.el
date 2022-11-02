;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Doom-related packages
(package! doom-modeline :pin "b66d5e5006df4cedb1119da3d83fd6c08965b83")

;; Org-related packages
(package! org-download)

;; Major modes
(package! debian-el)
(package! dockerfile-mode)
(package! dpkg-dev-el)
(package! protobuf-mode)
(package! puppet-mode)

;; Misc packages
(package! bm)
(package! git-auto-commit-mode)
(package! openbsd-knf-style :recipe (:host github :repo "hogand/openbsd-knf-emacs"))
