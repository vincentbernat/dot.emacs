;; el-get configuration
(setq el-get-is-lazy t)

(setq el-get-sources
      '((:name znc
               :type github
               :pkgname "sshirokov/ZNC.el"
               :description "ERC and ZNC interface")
        (:name edit-indirect
               :type github
               :pkgname "Fanael/edit-indirect"
               :description "Edit regions in separate buffers")
        (:name org-mode
               :checkout "release_9.1.13")
        (:name cider
               :checkout "v0.17.0")
        (:name clojure-mode
               :checkout "5.8.1")
        (:name bbdb
               :checkout "c951e15cd01d")
        (:name magit
               :checkout "2.13.0")))

(el-get nil
        '(
          ;; General
          naquadah-theme                ; Theme from Julien Danjou
          emojify                       ; Emojis
          multiple-cursors              ; multiple cursors
          avy                           ; fast cursor movement
          smex                          ; remember last M-x commands
          projectile                    ; handling of projects
          auto-compile                  ; automatically compile outdated stuff
          diminish                      ; remove stuff from modeline
          powerline                     ; powerline for mode-line
          spaceline                     ; spaceline (a theme for powerline)
          bm                            ; bookmarking
          swiper                        ; ivy, counsel, swiper
          edit-indirect                 ; edit region in separate buffers
          which-key                     ; display which keys is available
          ;; Programming
          paredit                       ; Parentheses management
          highlight-parentheses         ; Highlight parentheses surrounding the cursor
          rainbow-mode                  ; Display colors
          dtrt-indent                   ; Autodetect indentation
          magit                         ; Git stuff, must-have!
          ggtags                        ; gtags interface (etags on steroids)
          flycheck                      ; on-the-fly checker for many languages
          ;; Autocompletion
          company-mode                  ; company-mode
          ;; Modes
          auctex                        ; LaTeX mode
          coffee-mode                   ; Major mode for coffeescript
          lua-mode                      ; Major mode for lua
          markdown-mode                 ; Major mode for markdown
          php-mode                      ; Major mode PHP
          yaml-mode                     ; Major mode for YAML
          scala-mode                    ; Major mode for Scala
          go-mode                       ; Major mode for Go
          elpy                          ; Major mode for Python
          zencoding-mode                ; Mode to expand CSS tags to HTML
          actionscript-mode             ; Mode for actionscript
          systemtap-mode                ; Mode for systemtap
          erlang-mode                   ; Mode for Erlang
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
          systemd-mode                  ; Mode for systemd files
          slime                         ; Mode for Common Lisp
          groovy-emacs-mode             ; Mode for Groovy (incl gradle)
          nix-mode                      ; Mode for Nix
          ;; gnus and other stuff
          bbdb                          ; Big brother database
          gnus-identities               ; Manipulate Gnus identities
          ;; org stuff
          org-mode
          org-mime
          org-reveal
          org-passwords
          ;; Misc
          znc                           ; znc
          gist                          ; gist integration
          uuid                          ; compute UUID
          ))
