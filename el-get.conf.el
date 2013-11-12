;; el-get configuration
(setq el-get-is-lazy t)

(setq el-get-sources
      '((:name php-mode
               :after (progn
                        (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))))
        (:name systemtap-mode
               :description "Emacs mode for SystemTap."
               :type github
               :pkgname "ruediger/systemtap-mode")
	(:name actionscript-mode
	       :type github
	       :username "austinhaas"
	       :after (progn
                        (add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))))

        (:name git-modes
               :description "GNU Emacs modes for various Git-related files"
               :type github
               :pkgname "magit/git-modes")
        (:name magit
               :depends (git-modes))

        (:name znc
               :type github
               :pkgname "sshirokov/ZNC.el"
               :description "ERC and ZNC interface"
               :features znc)
        (:name gist
               :depends (gh))
        (:name gh
               :type github
               :pkgname "sigma/gh.el"
               :depends (pcache logito request)
               :description "Github API client libraries"
               :website "http://github.com/sigma/gh.el")
        (:name pcache
               :type github
               :pkgname "sigma/pcache"
               :description "persistent caching for Emacs"
               :website "http://github.com/sigma/pcache")
        (:name logito
               :type github
               :pkgname "sigma/logito"
               :description "logging library for Emacs"
               :website "http://github.com/sigma/logito")
        (:name request
               :description "Easy HTTP request for Emacs Lisp"
               :type github
               :submodule nil
               :pkgname "tkf/emacs-request")

        (:name ido-vertical-mode
               :type github
               :pkgname "rson/ido-vertical-mode.el"
               :description "makes ido-mode display vertically"
               :features ido-vertical-mode)
        (:name flx
               :description "Fuzzy matching with good sorting in ido"
               :type github
               :pkgname "lewang/flx"
               :features flx-ido)
        (:name s
               :description "The long lost Emacs string manipulation library."
               :type github
               :pkgname "magnars/s.el"
               :features s)
        (:name dash
               :description "A modern list api for Emacs. No 'cl required."
               :type github
               :pkgname "magnars/dash.el")
        (:name projectile
               :description "Project navigation and management library for Emacs"
               :type github
               :pkgname "bbatsov/projectile"
               :depends (dash s)
               :features projectile)))

(el-get nil
        '(
	  ;; General
	  naquadah-theme		; Theme from Julien Danjou
	  point-stack			; Organize points into a stack
          boxquote                      ; draw boxes
          multiple-cursors              ; multiple cursors
          ace-jump-mode                 ; fast cursor movement
          auto-complete                 ; universal autocompletion
          auto-complete-css
          flx                           ; fuzzy matching for ido
          ido-vertical-mode             ; vertical mode for ido
          smex                          ; IDO for M-x
          projectile                    ; handling of projects
          expand-region                 ; smartly expand region
	  ;; Programming
	  autopair			; Auto pairing of parentheses
          highlight-parentheses         ; Highlight parentheses surrounding the cursor
          rainbow-mode			; Display colors
	  dtrt-indent			; Autodetect indentation
	  magit				; Git stuff, must-have!
	  xcscope			; cscope interface (etags on steroids)
	  ;; Modes
	  auctex			; LaTeX mode
	  coffee-mode			; Major mode for coffeescript
	  lua-mode			; Major mode for lua
	  markdown-mode			; Major mode for markdown
	  php-mode			; Major mode PHP
          yaml-mode                     ; Major mode for YAML
          scala-mode                    ; Major mode for Scala
          go-mode                       ; Major mode for Go
          git-commit-mode               ; Mode for "git commit"
          zencoding-mode                ; Mode to expand CSS tags to HTML
          actionscript-mode             ; Mode for actionscript
          systemtap-mode                ; Mode for systemtap
	  ;; Gnus and other stuff
	  bbdb				; Big brother database
	  nognus			; Gnus
	  gnus-identities		; Manipulate Gnus identities
          ;; Misc
          znc                           ; znc
          gist                          ; gist integration
	  ))

(require 'scala-mode-auto)
