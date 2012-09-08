;; el-get configuration
(setq el-get-is-lazy t)

(el-get nil
        '(
	  ;; General
	  naquadah-theme		; Theme from Julien Danjou
	  point-stack			; Organize points into a stack
          boxquote                      ; draw boxes
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
          git-commit-mode               ; Mode for "git commit"
	  ;; Gnus and other stuff
	  bbdb				; Big brother database
	  nognus			; Gnus
	  gnus-identities		; Manipulate Gnus identities
	  ))
