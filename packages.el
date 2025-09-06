;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Packages can be updated by using M-x doom/bump-package-at-point. After
;; updating this file, you need to run 'doom sync -u'.

;; Major modes
(package! debian-el :pin "3f7c2b582eb65b47bea081f3c74ae70833c5af7e")
(package! dpkg-dev-el :pin "f2708f117cf69ff4c42858448b1101a27b5cb2a3")
(package! dockerfile-mode :pin "7ce17e054eca4d56ca8bc1e4a6a0dbf58efd8d52")
(package! lua-mode :pin "d074e4134b1beae9ed4c9b512af741ca0d852ba3")
(package! protobuf-mode :pin "2d6ab3ce45f85af6bffd694659148062ba9aecde")
(package! puppet-mode :pin "71bcd383f20a457e8ad34e0e08ec47f8e1b64263")
(package! junos-mode
  :recipe (:host github
           :repo "vincentbernat/junos-mode"
           :files (:defaults "junos.py"))
  :pin "e2e7f7224fff89f38f2b02f5431299d95654b380")
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")
(package! jinja2-mode :pin "03e5430a7efe1d163a16beaf3c82c5fd2c2caee1")
(package! yang-mode :pin "b7a4c1734a60f70d80d5752ae058232df0b18336")
(package! hcl-mode :pin "1da895ed75d28d9f87cbf9b74f075d90ba31c0ed")
(package! hurl-mode
  :recipe (:host github
           :repo "jaszhe/hurl-mode"
           :files ("*.el"))
  :pin "d0cb1dc52ae88238b20831ea7db05e0f332a3851")

;; Misc packages
(package! apheleia :pin "d8ccc0ba0f127c11df39e79313a17bcb740359c0")
(package! bm :pin "dcdc4bed0e966fd47393a649a1e0d7a25245a98e")
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
