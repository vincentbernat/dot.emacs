;;; vbe-programming.el --- Programming modes (and related)  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Vincent Bernat

;; Author: Vincent Bernat <bernat@luffy.cx>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'vbe-common)


;;; Utilities

;; First, magit!
(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-c g" . magit-file-dispatch)
  :pin "melpa"
  :commands magit-blame
  :after git-gutter-fringe
  :config
  ;; Add a "latest commits" section
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-recent-commits
                          nil t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  ;; Don't ask confirmation for style convention violations: they are
  ;; already highlighted by font locking.
  (remove-hook 'git-commit-finish-query-functions
               'git-commit-check-style-conventions)

  ;; Remove unneeded prompts
  (add-to-list 'magit-no-confirm 'stage-all-changes)

  ;; Refresh git-gutter
  (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)

  :custom
  (magit-completing-read-function 'ivy-completing-read)
  ;; Use M-x magit-describe-section-briefly to get a section name
  (magit-section-initial-visibility-alist
   '((stashes . hide)
     (unpushed . hide)
     (recent . show)
     (untracked . show)
     (unstaged . show))))

(use-package git-commit
  :config
  (global-git-commit-mode 1))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  :custom
  (git-gutter-fr:side 'left-fringe))

;; Then, flycheck. Needs to be enabled for each mode.
(use-package flycheck
  :pin "melpa"
  :custom
  ;; Use a dot file to avoid being detected by some watchers
  (flycheck-temp-prefix ".flycheck")
  ;; Do not hijack next-error
  (flycheck-standard-error-navigation nil)
  ;; Do not display anything in modeline (see spaceline)
  (flycheck-mode-line nil)
  ;; Display flycheck in right fringe
  (flycheck-indication-mode 'right-fringe)
  ;; Use Python3 for Python
  (flycheck-python-pycompile-executable "python3")

  :config
  ;; Enable globally
  (global-flycheck-mode 1)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center)

  ;; LISP: disable emacs-lisp-checkdoc.
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  (add-to-list 'flycheck-emacs-lisp-load-path (concat user-emacs-directory "lisp")))

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

;; Indentation detection.
(use-package dtrt-indent
  :diminish
  :hook (groovy-mode . dtrt-indent-mode)
  :defer 5
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list '(groovy-mode c/c++/java groovy-indent-offset))
  (dtrt-indent-global-mode 1))

;; Completion with company mode
(use-package company
  :diminish
  :config
  ;; Don't use up/down arrow (use M-n, M-p only) to browse list
  (unbind-key "<up>" company-active-map)
  (unbind-key "<down>" company-active-map)
  ;; Don't mess with space and enter
  (unbind-key "SPC" company-active-map)
  (bind-key "TAB" #'company-complete-selection company-active-map)
  (bind-key [C-return] #'counsel-company company-active-map)
  (dolist (key '("<return>" "RET"))
    ;; Here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. Doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
                  :filter ,(lambda (cmd)
                             (when (company-explicit-action-p)
                               cmd)))))
  ;; Enable globally
  (global-company-mode 1))

;; When compiling, colorize output
(use-package compile
  :bind (("<f9>" . recompile))
  :hook (compilation-filer . vbe:colorize-compilation-buffer)
  :config
  (require 'ansi-color)
  (defun vbe:colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :custom
  (compilation-ask-about-save nil))

;; Paredit for parenthesis
(use-package paredit
  :diminish (paredit-mode . "()")
  :hook ((cidr-repl-mode
          clojure-mode
          lisp-mode
          emacs-lisp-mode) . paredit-mode)
  :bind (:map paredit-mode-map
              ("M-k" . paredit-copy-as-kill))
  :config
  (with-eval-after-load "eldoc"
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;; Helpful for better help commands
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))

;; Eldoc
(use-package eldoc
  :diminish
  :defer t)

;; Abbrev
(use-package abbrev
  :ensure nil
  :diminish)

;; Electric indent
(use-package electric-indent
  :ensure nil
  :hook ((prog-mode . electric-indent-local-mode))
  :bind (("M-RET" . electric-indent-just-newline))
  :config
  (electric-indent-mode 0))

;; Display whitespaces.
(use-package whitespace
  :diminish
  :hook ((prog-mode markdown-mode html-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

;; Display colors.
(use-package rainbow-mode
  :hook (css-mode . rainbow-mode))

;; Highlight parentheses
(use-package highlight-parentheses
  :diminish
  :hook (prog-mode . highlight-parentheses-mode))

;; Gists
(use-package gist
  :defer t
  :custom
  (gist-view-gist t))

;; Ediff
(use-package ediff
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))


;;; Modes

(use-package cc-mode
  :hook ((c-mode . vbe:flycheck-fix-clang-include-path))
  :config
  (setq c-default-style "linux"         ; default style is "linux
        c-basic-offset 4)               ; with a 4 spaces indent

  (defun vbe:openbsd-knf-space-indent (langelem)
    "Indent either 4 spaces or none.

This function is from nicm@ and also in gsoares@'s config.
Most useful with c-offset-alist entries that are lists such as
arglist-cont-nonempty"
    (save-excursion
      (goto-char (cdr langelem))
      (while (let ((syntax (car (car (c-guess-basic-syntax)))))
               (or (eq syntax 'arglist-intro)
                   (eq syntax 'arglist-cont)
                   (eq syntax 'arglist-cont-nonempty)))
        (forward-line -1))
      (beginning-of-line)
      (re-search-forward "[^ \t]" (c-point 'eol))
      (goto-char (+ (match-beginning 0) 4))
      (vector (current-column))))

  ;; More styles. To debug, use C-c C-s.
  ;;  - `+` means `c-basic-offset` times 1.
  ;;  - `*` means `c-basic-offset` times 0.5.
  (c-add-style
   "openbsd"                     ; Should be compatible with FreeBSD too
   '(
     ;; General settings that should be enabled in c-mode
     (indent-tabs-mode . t)      ;; use tabs whenever feasible
     (fill-column . 80)          ;; Assume KNF tries to maintain 80 char lines
     (show-trailing-whitespace . t)  ;; KNF says to not have trailing WS
     (tab-width . 8)             ;; When displaying literal tab, show 8 spaces
     ;; c-mode
     (c-progress-interval . 1)   ;; display progress meter during long indents
     (c-basic-offset . 8)        ;; KNF uses 8 space tabs
     (c-comment-only-line-offset . 0)  ;; don't indent comments extra
     (c-backspace-function . delete-backward-char)
     (c-delete-function . delete-char)
     (c-syntactic-indentation-in-macros . t) ;; indent inside macro defs
     (c-tab-always-indent . t)  ;; indent line: Use C-q 'tab' to insert literal
     (c-continued-statement-offset 0)
     ;; (c-electric-flag . nil)   ;; if you don't like auto-indent
     (c-electric-continued-statement . t)
     ;;
     (c-indent-comments-syntactically-p . nil)
     ;;
     ;; Offsets for the various c-mode symbols.  Offsets are sometimes based
     ;; upon other offsets.  For instance, arglist-intro is the 1st argument
     ;; line.  If you define arglist-cont, it uses arglist-intro plus that.
     ;; c-echo-syntactic-information-p is your friend when debugging indents.
     ;;
     ;; [N] means absolute column.  All the others are relative.
     ;;  0 = no extra indentation.  For literal column 0, use [0]
     ;;  N = extra N spaces.  For literal column N, use [N]
     ;; ++ = c-basic-offset * 2
     ;; -- = c-basic-offset * -2
     ;;  + = c-basic-offset * 1
     ;;  - = c-basic-offset * -1
     ;;  * = c-basic-offset * 0.5
     ;;  / = c-basic-offset * -0.5
     (c-offsets-alist . (
                         ;; Literal symbols
                         (func-decl-cont . 0)        ; C++ style func mod
                         (block-open . 0)            ; '{' for block
                         (label . [1])               ; goto label in column 1
                         (comment-intro . 0)         ; C comment
                         (cpp-macro . [0])           ; #define in column 0
                         ;; Multiline macro symbols
                         (cpp-define-intro . [0])    ; first list = column 0
                         (cpp-macro-cont . +)        ; add'l lines in macro
                         ;; Function symbols
                         (defun-open . 0)            ; '{' alone for func
                         (defun-close . 0)           ; '}' alone for func
                         (defun-block-intro . +)     ; first line of func
                         (topmost-intro . 0)         ; outermost part
                         (topmost-intro-cont . 0)    ; outermost part cont
                         (statement . 0)             ; func stmt (already off)
                         ;; XXX statement-cont should be 4 unless
                         ;; it is part of a macro, then 8.
                         (statement-cont . *)        ; continue stmt
                         ;; Class symbols.  XXX Should add support since there
                         ;; is a little C++ in the tree (GNU)
                         ;; Java
                         ;; K&R
                         (knr-argdecl-intro . +)     ; rare K&R (from KNF)
                         (knr-argdecl . 0)           ; add'l indent for rest
                         ;; Conditional construct symbols
                         (block-close . 0)           ; '}' for block
                         (statement-block-intro . +) ; stmt in loop/cond
                         (substatement . +)          ; non-braced stmt if()
                         (substatement-open . 0)     ; '{' in loop/cond
                         (substatement-label . [1])  ; goto label in loop/cond
                         (do-while-closure . 0)      ; 'while' alone in 'do'
                         (else-clause . 0)           ; 'else' when not nested
                         ;; Brace list symbols
                         (brace-list-close . 0)      ; enum/agg list close
                         (brace-list-intro . +)      ; 1st line of enum/agg
                         (brace-list-entry . 0)      ; add'l indent for entries
                         (brace-list-open . 0)       ; enum/agg init open
                         ;; Switch statement symbols
                         (statement-case-open . +)   ; '{' in case
                         (statement-case-intro . +)  ; 1st line in case stmt
                         (case-label . 0)            ; case label in switch
                         ;; Paren list symbols
                         ;; XXX This is typically a list so need to handle it
                         ;; differently from the rest.  Emacs adds the indents.
                         (arglist-intro . vbe:openbsd-knf-space-indent) ; 1st line
                         (arglist-cont . vbe:openbsd-knf-space-indent)
                         (arglist-cont-nonempty . vbe:openbsd-knf-space-indent)
                         (arglist-close . 0)         ; ')' alone
                         ;; External scope symbols
                         (extern-lang-open . [0])    ; '{' alone in 'extern C'
                         (extern-lang-close . [0])   ; '}' alone in 'extern C'
                         (inextern-lang . +)         ; lines inside 'extern C'
                         ;; Statement block
                         (inexpr-statement . +)))    ; gcc extension stmt expr
     ;; If not specified, the default is "before after".  All variables are
     ;; defined here.
     (c-hanging-braces-alist . (
                                ;; All variables
                                (defun-open before after)  ; function, enum
                                (defun-close before after) ; function
                                (class-open after) ; struct too
                                (class-close before after)
                                (inline-open after)
                                (inline-close before after)
                                (block-open after)
                                (block-close . c-snug-do-while)
                                (statement-cont after)
                                (substatement-open after)
                                (statement-case-open before after)
                                (brace-list-open after)
                                (brace-list-close before close)
                                (brace-list-intro after)
                                (brace-entry-open after)
                                (extern-lang-open after)
                                (extern-lang-close before after)
                                (namespace-open after)           ;; C++
                                (namespace-close before afetr)   ;; C++
                                (module-open after)              ;; CORBA
                                (module-close before after)      ;; CORBA
                                (composition-open after)         ;; CORBA
                                (composition-close before after) ;; CORBA
                                (inexpr-class-open after)
                                (inexpr-class-close before after)
                                (arglist-cont-nonempty before after)))
     ;; Whether to auto-insert newline before/after colon
     (c-hanging-colons-alist . ((case-label after)
                                (label after)
                                (access-label after)  ;; C++
                                (member-init-intro before)
                                (inher-intro)))
     ;; Whether to insert newlines after ';' or ','
     (c-hanging-semi&comma-criteria . (
                                       ;; supress newline when next line non-blank
                                       c-semi&comma-no-newlines-before-nonblanks
                                       ;; suppress newline in paren (for loop etc)
                                       c-semi&comma-inside-parenlist
                                       ;; supress newline for one liner
                                       c-semi&comma-no-newlines-for-oneline-inliners))
     ;; When autonewline mode is enabled, clean up some extra newlines
     (c-cleanup-list . (brace-else-brace    ; } else {
                        brace-elseif-brace  ; } else if {
                        brace-catch-brace   ; } catch (...) {
                        ;; empty-defun-braces ; {} instead of multiple lines
                        defun-close-semi    ; struct: no \n between '}' and ';'
                        list-close-comma    ; remove final comma
                        scope-operator
                        ;; space-before-funcall ; GNU standard
                        ;; compact-empty-funcall ; another GNU standard
                        ;; comment-close-slash ; term comment with slash
                        ))))

  (defun vbe:flycheck-fix-clang-include-path ()
    "Setup include path to also look in alternate directories"
    (require 'projectile)
    (let* ((root (if (projectile-project-p)
                     (projectile-project-root)
                   default-directory))
           (altroot (locate-dominating-file default-directory "configure.ac"))
           (additionals (list root
                              (f-join root "build")
                              (f-join root "%sbuild~")
                              (and altroot (expand-file-name altroot))
                              (and altroot (f-join altroot "build"))
                              (and altroot (f-join altroot "build~"))))
           (candidates (-filter 'file-accessible-directory-p (-non-nil additionals))))
      (setq-local flycheck-clang-include-path (-distinct candidates))))

  (setq c-font-lock-extra-types (-union c-font-lock-extra-types
                                        '("Gdk\\sw+" "Gtk\\sw+"
                                          "gchar" "gboolean" "guchar"
                                          "gshort" "gushort" "glong" "gulong"
                                          "gint" "gint8" "gint16" "gint32" "gint64"
                                          "guint" "guint8" "guint16" "guint32" "guint64"
                                          "glong" "gdouble" "goffset"
                                          "gsize" "gssize"
                                          "gpointer" "guintptr"))))

(use-package js2-mode
  :interpreter "node"
  :mode "\\.js\\'"
  :custom
  (js2-skip-preprocessor-directives t)
  (js-indent-level 2)

  ;; Let flycheck handle errors.
  (js2-strict-missing-semi-warning nil))

(use-package json-mode
  :defer t)

(use-package css-mode
  :mode ("\\.css\\'" "\\.less\\'"))

(use-package web-mode
  :mode ("\\.jsx\\'" "\\.html?\\'")
  :custom
  (web-mode-enable-auto-indentation nil)
  :config
  (setq web-mode-engines-alist
        '(("django" . "\\.j2\\'"))))

(use-package auctex
  :defer t)

(use-package lua-mode
  :pin "melpa"
  :defer t)

(use-package markdown-mode
  :pin "melpa"
  :mode ("\\.md\\'"
         "\\.markdown\\'"
         ("README.md\\'" . gfm-mode))
  :custom
  (markdown-spaces-after-code-fence 0)
  (markdown-footnote-location 'immediately)
  (markdown-reference-location 'end)
  (markdown-gfm-use-electric-backquote nil))

(use-package ruby-mode
  :defer t)

(use-package sh-mode
  :ensure nil
  :mode "\\.zsh'")

(use-package yaml-mode
  :pin "melpa"
  :defer t)

(use-package hcl-mode
  :defer t)

(use-package go-mode
  :defer t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :custom
  ;; Change the spinner type to stay at constant-width
  (cider-eval-spinner-type 'vertical-breathing))
(use-package clojure-mode
  :defer t
  :config
  ;; Some indentation preferences (for midje)
  (put-clojure-indent 'fact 1)
  (put-clojure-indent 'facts 1)

  ;; Alignment for :as
  (with-eval-after-load "align"
    (add-to-list 'align-rules-list
                 '(clojure-require-refer-and-as
                   (regexp   . "\\[[^]]*\\(\\s-\\s-*\\)\\(:as\\|:refer\\)\\s-*[^]]*\\]")
                   (group    . 1)
                   (modes    . '(clojure-mode))
                   (tab-stop . nil)))))

;; Evaluate last sexp "inline", like with CIDER
(use-package eros
  :config
  (eros-mode 1))

(use-package dockerfile-mode
  :pin "melpa"
  :defer t)

(use-package puppet-mode
  :commands puppet-align-block
  :bind (:map puppet-mode-map
              ("C-c |" . puppet-align-block)))

(use-package salt-mode
  :defer t)

(use-package debian-changelog-mode
  :ensure nil
  :defer t
  :custom
  (debian-changelog-mailing-address (s-join "@" '("bernat" "debian.org")))
  :config
  ;; Add UNRELEASED at the front place
  (custom-set-variables '(debian-changelog-allowed-distributions
                          (-rotate 1 debian-changelog-allowed-distributions))))

(use-package groovy-mode
  :defer t)

(use-package snmp-mode
  :mode (("-MIB\\'" . snmpv2-mode)
         ("-MIB\\.txt\\'" . snmpv2-mode)))

(use-package rust-mode
  :defer t)

(use-package php-mode
  :defer t)

(use-package nix-mode
  :defer t)

(use-package toml-mode
  :defer t)

(use-package protobuf-mode
  :mode ("\\.pb\\'"))

(use-package cmake-mode
  :defer t)

(use-package junos-mode
  :quelpa (junos-mode :fetcher github
                      :repo "vincentbernat/junos-mode"
                      :files (:defaults "junos.py"))
  :defer t)

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package realgud
  :commands realhud:gdb
  :defer t)

(use-package pyvenv
  :defer t)

(use-package dumb-jump
  :bind (("M-g ." . dumb-jump-go)
         ("M-g ," . dumb-jump-back))
  :config
  (require 'projectile)
  (advice-add 'dumb-jump-get-project-root :override #'projectile-project-root)
  :custom
  (dumb-jump-max-find-time 10)
  (dumb-jump-selector 'ivy))


;;; Emacs LSP
(use-package lsp-mode
  :defer t
  :custom
  (lsp-enable-snippet nil)
  (lsp-prefer-flymake nil)
  (lsp-pyls-plugins-pylint-enabled nil)
  :config
  (require 'projectile))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook ((lsp-mode . lsp-ui-mode))
  :custom
  (lsp-ui-sideline-delay 0.8 "wait a bit before showing sideline"))

(use-package cquery
  :after (lsp-mode)
  :config
  (require 'projectile)
  (add-to-list 'cquery-project-root-matchers "build~/compile_commands.json")
  :custom
  (cquery-cache-dir ".cquery_cached_index~/")
  (cquery-executable "nice")
  (cquery-extra-args (list (vbe:executable-path "cquery")))
  (cquery-extra-init-params '(:compilationDatabaseDirectory "build~"
                                                            :cacheFormat "msgpack")))

(use-package company-lsp
  :after (lsp-mode)
  :config
  (push 'company-lsp company-backends))

(provide 'vbe-programming)
;;; vbe-programming.el ends here
