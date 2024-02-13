;;; init.el -*- lexical-binding: t; -*-

;; NOTE Use `doom sync' after a modification.

;; NOTE Use `C-c c k' to view a module documentation. Use `C-c c d' to view its
;;      directory (and source code). Use `C-h d m' to get the list of available
;;      modules.

(setenv "LSP_USE_PLISTS" "true")           ; use plists for LSP

(doom! :completion
       (company +childframe)
       (vertico +icons)

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
       (dired +icons)
       electric
       vc

       :checkers
       (syntax +childframe)

       :tools
       direnv
       editorconfig
       (eval +overlay)
       (lookup +docsets)
       lsp
       magit

       :lang
       beancount
       (cc +lsp)
       emacs-lisp
       (go +lsp)
       json
       (javascript +lsp)
       markdown
       nix
       (org +present)
       php
       (python +lsp +pyright)
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
