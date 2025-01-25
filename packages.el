;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Packages can be updated by using M-x doom/bump-package-at-point. After
;; updating this file, you need to run 'doom sync -u'.

;; Major modes
(package! debian-el :pin "3f7c2b582eb65b47bea081f3c74ae70833c5af7e")
(package! dpkg-dev-el :pin "f2708f117cf69ff4c42858448b1101a27b5cb2a3")
(package! dockerfile-mode :pin "4d893bd2da15833ce056332e6c972d5d93e78f04")
(package! lua-mode :pin "d074e4134b1beae9ed4c9b512af741ca0d852ba3")
(package! protobuf-mode :pin "ae3015c78a0ea3baa76f3ad556c0ecd575f2e618")
(package! puppet-mode :pin "71bcd383f20a457e8ad34e0e08ec47f8e1b64263")
(package! junos-mode
  :recipe (:host github
           :repo "vincentbernat/junos-mode"
           :files (:defaults "junos.py"))
  :pin "e2e7f7224fff89f38f2b02f5431299d95654b380")
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
(package! yang-mode :pin "4b4ab4d4a79d37d6c31c6ea7cccbc425e0b1eded")

;; Misc packages
(package! apheleia :pin "e112fe1bf4fdaef1c9ab741590ac45b06dc01f76")
(package! bm :pin "1351e2e15a7666e614c94b41414c8f024dc10a50")
(package! git-auto-commit-mode :pin "a7b59acea622a737d23c783ce7d212fefb29f7e6")
(package! openbsd-knf-style
  :recipe (:host github
           :repo "vincentbernat/openbsd-knf-emacs")
  :pin "1144dd016dd0e0fd096073db2492bb5814e8ada8")
(package! pass :pin "1a9f6100153b07ac4f4d1d332501240e94c38f1e")
(package! org-download :pin "c8be2611786d1d8d666b7b4f73582de1093f25ac")

;; Disable some packages I don't want
(package! tide :disable t)              ; I am using LSP
(package! anaconda-mode :disable t)     ; I am using LSP
(package! emmet-mode :disable t)        ; In web-mode, prevent completion in <script>
