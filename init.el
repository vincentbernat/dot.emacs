;;; init.el -*- lexical-binding: t; -*-

;; NOTE Use `doom sync' after a modification.

;; NOTE Use `C-c c k' to view a module documentation. Use `C-c c d' to view its
;;      directory (and source code). Use `C-h d m' to get the list of available
;;      modules.

(setenv "LSP_USE_PLISTS" "1")           ; use plists for LSP

(doom! :completion
       (company +childframe)
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
       lispy
       multiple-cursors
       snippets

       :emacs
       dired
       electric
       vc

       :checkers
       (syntax +childframe)

       :tools
       direnv
       editorconfig
       (eval +overlay)
       (lookup +docsets)
       (lsp +peek)
       magit

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
