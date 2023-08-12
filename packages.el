;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Packages can be updated by using M-x doom/bump-package-at-point.

;; Major modes
(package! debian-el :pin "a3ef20c269b9192710567571b20718f572942bc4")
(package! dpkg-dev-el :pin "af9aad721cb263e495e2f77df458e9496549c04b")
(package! dockerfile-mode :pin "52c6c00da1d31c0b6c29c74335b3af63ed6bf06c")
(package! lua-mode :pin "ad639c62e38a110d8d822c4f914af3e20b40ccc4")
(package! protobuf-mode :pin "dd4ffc65cc78347b2c6a15828f1a0edb129a44d0")
(package! puppet-mode :pin "71bcd383f20a457e8ad34e0e08ec47f8e1b64263")
(package! junos-mode
  :recipe (:host github
           :repo "vincentbernat/junos-mode"
           :files (:defaults "junos.py"))
  :pin "e2e7f7224fff89f38f2b02f5431299d95654b380")
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
(package! adoc-mode :pin "a7691c8b9a738fd724007a2a283ed2c20684a7e5")
(package! yang-mode :pin "4b4ab4d4a79d37d6c31c6ea7cccbc425e0b1eded")

;; Misc packages
(package! apheleia :pin "c222927f7086d407dad01b2609ff68768e9adddb")
(package! bm :pin "9a31c61f44e6f1033ca43bd7f3eb33ffdb2ca595")
(package! git-auto-commit-mode :pin "a7b59acea622a737d23c783ce7d212fefb29f7e6")
(package! openbsd-knf-style
  :recipe (:host github
           :repo "vincentbernat/openbsd-knf-emacs")
  :pin "1144dd016dd0e0fd096073db2492bb5814e8ada8")
(package! pass :pin "5651da53137db9adcb125b4897c2fe27eeb4368d")
(package! org-download :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7")

;; Disable some packages I don't want
(package! tide :disable t)              ; I am using LSP
(package! anaconda-mode :disable t)     ; I am using LSP
(package! emmet-mode :disable t)        ; In web-mode, prevent completion in <script>
