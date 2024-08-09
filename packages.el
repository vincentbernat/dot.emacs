;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Packages can be updated by using M-x doom/bump-package-at-point. After
;; updating this file, you need to run 'doom sync -u'.

;; Major modes
(package! debian-el :pin "232a4b3f20ca7834a8993c4aac2678b7aecca505")
(package! dpkg-dev-el :pin "eb00c58726be5e914b2487bccf7e3f716e7a61e1")
(package! dockerfile-mode :pin "39a012a27fcf6fb629c447d13b6974baf906714c")
(package! lua-mode :pin "d074e4134b1beae9ed4c9b512af741ca0d852ba3")
(package! protobuf-mode :pin "903c3f15b04d99ab88cee53e4cec9464ef292bce")
(package! puppet-mode :pin "71bcd383f20a457e8ad34e0e08ec47f8e1b64263")
(package! junos-mode
  :recipe (:host github
           :repo "vincentbernat/junos-mode"
           :files (:defaults "junos.py"))
  :pin "e2e7f7224fff89f38f2b02f5431299d95654b380")
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
(package! adoc-mode :pin "2c2eb8043623aa99d35aacbad2ee39188bf1bad3")
(package! yang-mode :pin "4b4ab4d4a79d37d6c31c6ea7cccbc425e0b1eded")

;; Misc packages
(package! apheleia :pin "3e347cff47eb0072b47c56f6188c4f440e477770")
(package! bm :pin "1351e2e15a7666e614c94b41414c8f024dc10a50")
(package! git-auto-commit-mode :pin "a7b59acea622a737d23c783ce7d212fefb29f7e6")
(package! openbsd-knf-style
  :recipe (:host github
           :repo "vincentbernat/openbsd-knf-emacs")
  :pin "1144dd016dd0e0fd096073db2492bb5814e8ada8")
(package! pass :pin "ed7031c5c33a384d07da2d15c9d5f854027a26a2")
(package! org-download :pin "19e166f0a8c539b4144cfbc614309d47a9b2a9b7")

;; Disable some packages I don't want
(package! tide :disable t)              ; I am using LSP
(package! anaconda-mode :disable t)     ; I am using LSP
(package! emmet-mode :disable t)        ; In web-mode, prevent completion in <script>
