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
        (:name flx
               :description "Fuzzy matching with good sorting in ido"
               :type github
               :pkgname "lewang/flx"
               :features flx-ido)
	(:name actionscript-mode
	       :type github
	       :username "austinhaas"
	       :after (progn
                        (add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))))))

(el-get nil
        '(
	  ;; General
	  naquadah-theme		; Theme from Julien Danjou
	  point-stack			; Organize points into a stack
          boxquote                      ; draw boxes
          multiple-cursors              ; multiple cursors
          ace-jump-mode                 ; fast cursor movement
          auto-complete                 ; universal autocompletion
          flx                           ; fuzzy matching for ido
	  ;; Programming
	  autopair			; Auto pairing of parentheses
          highlight-parentheses         ; Highlight parentheses surrounding the cursor
          rainbow-mode			; Display colors
	  dtrt-indent			; Autodetect indentation
	  magit				; Git stuff, must-have!
	  xcscope			; cscope interface (etags on steroids)
          fill-column-indicator         ; show fill column
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
	  ))

(require 'scala-mode-auto)
