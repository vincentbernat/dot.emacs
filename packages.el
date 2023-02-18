;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Packages can be updated by using M-x doom/bump-package-at-point.

;; Major modes
(package! debian-el :pin "a3ef20c269b9192710567571b20718f572942bc4")
(package! dpkg-dev-el :pin "458f5230d02b15c94e94eca1af4eabaec30f45db")
(package! dockerfile-mode :pin "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")
(package! lua-mode :pin "3e783c93aa8a3d3ca985686438aa8d140cbddae6")
(package! protobuf-mode :pin "c862c1cab4d591963437774cc7d3f0a8e4bb1da1")
(package! puppet-mode :pin "71bcd383f20a457e8ad34e0e08ec47f8e1b64263")
(package! junos-mode
  :recipe (:host github
           :repo "vincentbernat/junos-mode"
           :files (:defaults "junos.py"))
  :pin "e2e7f7224fff89f38f2b02f5431299d95654b380")
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")

;; Misc packages
(package! apheleia :pin "33d4542b58476d50f01464576664de1acea1f62f")
(package! bm :pin "9a31c61f44e6f1033ca43bd7f3eb33ffdb2ca595")
(package! git-auto-commit-mode :pin "a6b6e0fa183be381463e2b44ef128db1b6c4234b")
(package! openbsd-knf-style
  :recipe (:host github
           :repo "vincentbernat/openbsd-knf-emacs")
  :pin "1144dd016dd0e0fd096073db2492bb5814e8ada8")
(package! lsp-mode :pin "f5d521d56cfef54d0f102680e956a856347d2c96")
(package! pass :pin "5651da53137db9adcb125b4897c2fe27eeb4368d")
(package! org-download :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7")

;; Disable some packages I don't want
(package! tide :disable t)              ; I am using LSP
(package! anaconda-mode :disable t)     ; I am using LSP
(package! emmet-mode :disable t)        ; In web-mode, prevent completion in <script>
