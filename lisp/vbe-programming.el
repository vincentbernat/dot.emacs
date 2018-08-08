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
  :bind (("C-x g" . magit-status))
  :config
  ;; Add a "latest commits" section
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-recent-commits
                          nil t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent t)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

  (remove-hook 'git-commit-finish-query-functions
               'git-commit-check-style-conventions)

  ;; Remove unneeded prompts
  (add-to-list 'magit-no-confirm 'stage-all-changes)

  (setq magit-completing-read-function 'ivy-completing-read))

;; Then, flycheck. Needs to be enabled for each mode.
(use-package flycheck
  :config
  (setq
   ;; Use a dot file to avoid being detected by some watchers
   flycheck-temp-prefix ".flycheck"
   ;; Do not hijack next-error
   flycheck-standard-error-navigation nil
   ;; Do not display anything in modeline (see spaceline)
   flycheck-mode-line nil
   ;; Don't enable flycheck on some modes
   flycheck-global-modes '(not erc-mode))

  ;; Enable globally
  (global-flycheck-mode 1)

  ;; Go: speedup compilation by saving intermediate files.
  (setq flycheck-go-build-install-deps t)
  ;; LISP: disable emacs-lisp-checkdoc.
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
  (add-to-list 'flycheck-emacs-lisp-load-path (concat user-emacs-directory "lisp")))

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
  (setq compilation-ask-about-save nil))

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
  :hook ((prog-mode . electric-indent-mode)))

;; Display whitespaces.
(use-package whitespace
  :diminish
  :hook ((prog-mode markdown-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing)))

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
  :config
  (setq gist-view-gist t))


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
  :config
  (setq js2-skip-preprocessor-directives t)
  (setq-default js2-basic-offset 2)

  ;; Let flycheck handle errors.
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil))

(use-package json-mode
  :mode "\\.json\\'")

(use-package css-mode
  :mode ("\\.css\\'" "\\.less\\'"))

(use-package web-mode
  :mode ("\\.jsx\\'" "\\.html?\\'")
  :config
  (setq web-mode-engines-alist
        '(("django" . "\\.j2\\'"))))

(use-package auctex
  :mode "\\.tex\\'")

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"
         ("README.md\\'" . gfm-mode)))

(use-package ruby-mode
  :mode ("\\.rb\\'" "/Rakefile\\'"))

(use-package sh-mode
  :ensure nil
  :mode "\\.zsh'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package go-mode
  :mode "\\.go\\'"
  :config
  ;; I am allergic to the GOPATH concept. I use kludge to work around them.
  (defun vbe:custom-gopath ()
    "Guess gopath if we have a .gopath or .gopath~ directory."
    (-slice (-non-nil (-map (lambda (gopath)
                              (-when-let (d (locate-dominating-file buffer-file-name gopath))
                                (f-join d gopath)))
                            '(".gopath" ".gopath~")))
            0 1))
  (defun vbe:custom-go-executable ()
    "Guess a custom Go executable"
    (-when-let (gopath (-first-item (vbe:custom-gopath)))
      (let ((go-exec (f-join (f-dirname gopath) "go~")))
        (when (not (f-exists? go-exec))
          (f-write-text (format "#!/bin/sh

# Set GOPATH
export GOPATH=%s

# Translate the current directory into a directory in GOPATH
current=$(realpath --relative-to=$GOPATH/.. $PWD)
for d in $(find $GOPATH/src -type l -print); do
    readlink -f $d | grep -qFx $(readlink -f $GOPATH/..) && {
        newroot=$d
        break
    }
done
target=$newroot/$current
[ -d  ] && cd $target

exec go \"$@\"
" gopath) 'utf-8 go-exec)
          (set-file-modes go-exec #o755))
        (setq-local flycheck-go-build-executable go-exec))))
  (add-hook 'go-guess-gopath-functions #'vbe:custom-gopath)
  (add-hook 'go-mode-hook #'vbe:custom-go-executable)
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq
   ;; Change the spinner type to stay at constant-width
   cider-eval-spinner-type 'vertical-breathing))
(use-package clojure-mode
  :mode "\\.clj\\'"
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
  :mode "Dockerfile\\'")

(use-package puppet-mode
  :mode "\\.pp\\'"
  :commands (puppet-align-block)
  :bind (:map puppet-mode-map
              ("C-c |" . puppet-align-block)))

(use-package debian-changelog-mode
  :ensure nil
  :defer t
  :config
  (setq debian-changelog-mailing-address "bernat@debian.org")

  ;; Add UNRELEASED at the front place
  (setq debian-changelog-allowed-distributions
        (-rotate 1 debian-changelog-allowed-distributions)))

(use-package groovy-mode
  :mode ("\\.gradle\\'" "\\.groovy\\'"))

(use-package snmp-mode
  :mode ("-MIB\\'" . snmpv2-mode))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package toml-mode
  :mode ("\\.toml\\'"))

(use-package junos-mode
  :quelpa (junos-mode :fetcher github
                      :repo "vincentbernat/junos-mode"
                      :files (:defaults "junos.py"))
  :commands junos-mode)

(use-package realgud
  :commands (realhud:gdb)
  :defer t)


;;; Emacs LSP
(use-package lsp-mode
  :defer t
  :config
  (require 'projectile))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook ((lsp-mode . lsp-ui-mode))
  :config
  ;; Don't show flycheck errors in sideline.
  (setq lsp-ui-sideline-show-flycheck nil)
  ;; Wait a bit before showing sideline
  (setq lsp-ui-sideline-delay 0.8))

(use-package cquery
  :ensure-system-package (cquery . "nix-env -i cquery")
  :hook ((c-mode . vbe:lsp-cquery-enable)
         (c++-mode . vbe:lsp-cquery-enable))
  :config
  (require 'projectile)
  (defun vbe:lsp-cquery-enable ()
    "Enable cquery, only for specific conditions."
    (when (and
           ;; Only for true C/C++ modes
           (-contains? '(c-mode cc-mode) major-mode)
           ;; For some projects, check we have a compile_commands.json
           ;; at the root. This file can be generated with "bear
           ;; make".
           (or (not (-contains? '("linux") (projectile-project-name)))
               (f-exists? (f-join (projectile-project-root) "compile_commands.json"))))
      (lsp-cquery-enable)))
  (setq cquery-cache-dir ".cquery_cached_index~/"
        cquery-executable "nice"
        cquery-extra-args '("cquery")
        cquery-extra-init-params '(:cacheFormat "msgpack")))

(use-package company-lsp
  :after (lsp-mode)
  :config
  (push 'company-lsp company-backends))

(provide 'vbe-programming)
;;; vbe-programming.el ends here
