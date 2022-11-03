;;; init.el -*- lexical-binding: t; -*-

;; NOTE Use `doom sync' after a modification.

;; NOTE Use `C-c c k' to view a module documentation. Use `C-c c d' to view its
;;      directory (and source code). Use `C-h d m' to get the list of available
;;      modules.

(doom! :completion
       company
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo
       indent-guides
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       vi-tilde-fringe

       :editor
       fold
       format
       lispy
       multiple-cursors
       snippets
       word-wrap

       :emacs
       dired
       electric
       vc

       :checkers
       syntax

       :tools
       direnv
       editorconfig
       (eval +overlay)
       lookup
       lsp
       magit
       pass

       :lang
       (cc +lsp)
       emacs-lisp
       (go +lsp)
       json
       (javascript +lsp)
       markdown
       nix
       (org +present)
       (python +lsp)
       rst
       sh
       web
       (yaml +lsp)

       :email
       :app
       :os
       :term
       :input

       :config
       (default +bindings +smartparens))
