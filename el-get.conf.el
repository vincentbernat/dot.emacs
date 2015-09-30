;; el-get configuration
(setq el-get-is-lazy t)

(setq el-get-sources
      '(;; This should be in contrib/ of org-mode but this is not
        ;; shipped with Emacs. We take exactly the version we need to
        ;; match org-mode in Emacs.
        (:name org-mime
               :description "org html export for text/html MIME emails"
               :type http
               :url "https://raw.github.com/jwiegley/org-mode/release_7.9.3f/contrib/lisp/org-mime.el")
        (:name org-bullets
               :description "utf-8 bullets for org-mode"
               :type github
               :pkgname "sabof/org-bullets")

        (:name znc
               :type github
               :pkgname "sshirokov/ZNC.el"
               :description "ERC and ZNC interface")))

(el-get nil
        '(
	  ;; General
	  naquadah-theme		; Theme from Julien Danjou
	  point-stack			; Organize points into a stack
          boxquote                      ; draw boxes
          multiple-cursors              ; multiple cursors
          avy                           ; fast cursor movement
          flx                           ; fuzzy matching
          swiper                        ; search with context (contains ivy)
          smex                          ; IDO for M-x
          projectile                    ; handling of projects
          auto-compile                  ; automatically compile outdated stuff
          browse-kill-ring              ; browse kill ring
          diminish                      ; remove stuff from modeline
          powerline                     ; powerline for mode-line
	  ;; Programming
          paredit                       ; Parentheses management
          highlight-parentheses         ; Highlight parentheses surrounding the cursor
          rainbow-mode			; Display colors
	  dtrt-indent			; Autodetect indentation
	  magit				; Git stuff, must-have!
	  ggtags			; gtags interface (etags on steroids)
          flycheck                      ; on-the-fly checker for many languages
          pretty-mode                   ; pretty symbols for many modes
          ;; Autocompletion
          company-mode                  ; company-mode
          company-anaconda              ; completion for Python
	  ;; Modes
	  auctex			; LaTeX mode
	  coffee-mode			; Major mode for coffeescript
	  lua-mode			; Major mode for lua
	  markdown-mode			; Major mode for markdown
	  php-mode			; Major mode PHP
          yaml-mode                     ; Major mode for YAML
          scala-mode                    ; Major mode for Scala
          go-mode                       ; Major mode for Go
          zencoding-mode                ; Mode to expand CSS tags to HTML
          actionscript-mode             ; Mode for actionscript
          systemtap-mode                ; Mode for systemtap
          vala-mode                     ; Mode for Vala
          erlang-mode                   ; Mode for Erlang
          mediawiki                     ; Mode for mediawiki
          gyp-mode                      ; Mode for gyp files
          dockerfile-mode               ; Mode for Dockerfile
          json-mode                     ; Mode for JSON
          apache-mode                   ; Mode for Apache configuration files
          web-mode                      ; Mode for web stuff (better than html-mode)
          scss-mode                     ; Mode for SCSS files
          js2-mode                      ; Mode for Javascript
          po-mode                       ; Mode for PO files
          ledger-mode                   ; Mode for ledger
          clojure-mode                  ; Mode for clojure
          cider                         ; REPL for cider
          midje-mode                    ; Mode for Midje (clojure stuff)
          puppet-mode                   ; Mode for puppet
          junos-mode                    ; Mode for JunOS
          cisco-router-mode             ; Mode for Cisco
          haskell-mode                  ; Mode for haskell
          flycheck-haskell              ; Flycheck for haskell
          protobuf-mode                 ; Mode for protobuf
          bison-mode                    ; Mode for bison
          rust-mode                     ; Mode for Rust
          toml-mode                     ; Mode for toml files
          slime				; Mode for Common Lisp
	  ;; gnus and other stuff
	  bbdb				; Big brother database
	  gnus				; Gnus
	  gnus-identities		; Manipulate Gnus identities
          ;; org stuff
          org-mime
          org-bullets
          org-passwords
          ;; Misc
          znc                           ; znc
          gist                          ; gist integration
	  ))
