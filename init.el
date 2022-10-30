;;; init.el -*- lexical-binding: t; -*-

;; NOTE Use `doom sync' after a modification.

;; NOTE Use `C-c c k' to view a module documentation. Use `C-c c d' to view its
;;      directory (and source code). Use `C-h d m' to get the list of available
;;      modules.

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
       (format +onsave)
       lispy
       multiple-cursors
       word-wrap

       :emacs
       dired
       electric
       vc

       :checkers
       (syntax +childframe)

       :tools
       editorconfig
       (eval +overlay)
       lookup
       lsp
       magit
       pass
       tree-sitter

       :lang
       (cc +tree-sitter)
       emacs-lisp
       (go +lsp +tree-sitter)
       (json +tree-sitter)
       (javascript +tree-sitter)
       markdown
       nix
       (org +present)
       (python +tree-sitter)
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
