;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Packages can be updated by using M-x doom/bump-package-at-point. After
;; updating this file, you need to run 'doom sync -u'.

;; Major modes
(package! debian-el :pin "6b7cb423527067438d50eb46e63616b8acf4b251")
(package! dpkg-dev-el :pin "719c1dbea93f4b524937d280f18871981714a012")
(package! dockerfile-mode :pin "4d893bd2da15833ce056332e6c972d5d93e78f04")
(package! lua-mode :pin "d074e4134b1beae9ed4c9b512af741ca0d852ba3")
(package! protobuf-mode :pin "5d0865cf1537772b8e0969563402654087b40d31")
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
(package! apheleia :pin "543f6d651d11322f26665f017f051fbcfc21ceb0")
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
