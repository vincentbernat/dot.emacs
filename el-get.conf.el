;; el-get configuration
(setq el-get-is-lazy t)

(setq el-get-sources
      '((:name php-mode
               :after (progn
                        (add-to-list 'auto-mode-alist '("\\.php$" . php-mode))))
	(:name actionscript-mode
	       :type github
	       :username "austinhaas"
	       :after (progn
                        (add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))))
        (:name vala-mode
               :description "Emacs mode for Vala language"
               :type elpa
               :after (progn
                        (add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))))
        (:name erlang-mode
               :description "Major mode for editing and running Erlang"
               :type http
               :url "http://www.erlang.org/download/contrib/erlang.el"
               :after (progn
                        (add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))))
        (:name gyp-mode
               :description "Major mode for editing gyp files"
               :type http
               :url "https://gyp.googlecode.com/svn/trunk/tools/emacs/gyp.el"
               :prepare (progn
                          (autoload 'gyp-mode "gyp" "Major mode for editing gyp files")
                          (add-to-list 'auto-mode-alist '("\\.gyp$" . gyp-mode))
                          (add-to-list 'auto-mode-alist '("\\.gypi$" . gyp-mode))))
        (:name mediawiki
               :after (progn
                        (autoload 'mediawiki-mode "mediawiki.el"
                          "Mode for mediawiki")
                        (add-to-list 'auto-mode-alist '("\\.mw$" . mediawiki-mode))))

        ;; This should be in contrib/ of org-mode but this is not
        ;; shipped with Emacs. We take exactly the version we need to
        ;; match org-mode in Emacs.
        (:name org-mime
               :description "org html export for text/html MIME emails"
               :type http
               :url "https://raw.github.com/jwiegley/org-mode/release_7.9.3f/contrib/lisp/org-mime.el")
        (:name org-bullets
               :description "utf-8 bullets for org-mode"
               :type github
               :username "sabof")

        (:name geben
               :website "https://code.google.com/p/geben-on-emacs/"
               :type svn
               :url "http://geben-on-emacs.googlecode.com/svn/trunk/")

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
	  ggtags			; gtags interface (etags on steroids)
          geben                         ; emacs-on-geben (debugger for PHP)
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
          vala-mode                     ; Mode for Vala
          erlang-mode                   ; Mode for Erlang
          mediawiki                     ; Mode for mediawiki
          gyp-mode                      ; Mode for gyp files
          dockerfile-mode               ; Mode for Dockerfile
	  ;; gnus and other stuff
	  bbdb				; Big brother database
	  gnus				; Gnus
	  gnus-identities		; Manipulate Gnus identities
          ;; org stuff
          org-mime
          org-bullets
          ;; Misc
          znc                           ; znc
          gist                          ; gist integration
	  ))

(require 'scala-mode-auto)
