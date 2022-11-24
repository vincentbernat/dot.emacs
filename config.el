;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Set font and theme
(setq doom-font (font-spec :family "Iosevka Term SS18" :size 11.0)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 11.0))
(setq doom-theme 'doom-vibrant)
;; Customize dashbord
(remove-hook '+doom-dashboard-functions 'doom-dashboard-widget-footer)
(setq +doom-dashboard-menu-sections
      (seq-filter (fn! (not (seq-contains-p '("Open org-agenda" "Jump to bookmark") (car %))))
                  +doom-dashboard-menu-sections))
(setq fancy-splash-image (concat doom-user-dir "doom-emacs.svg"))
;; Don't display line numbers
(setq display-line-numbers-type nil)
;; Kill at the beginning of a line should also remove the line
(setq kill-whole-line t)
;; Do not delete selection
(delete-selection-mode -1)
;; Always indent with tab (Doom Emacs set it to nil)
(setq-default tab-always-indent 'complete)
;; Do not continue comments (implement differs between modes, this is confusing)
(setq +default-want-RET-continue-comments nil)
;; Restore line continuation
(setq-default word-wrap nil)
(setq-default truncate-lines nil)

(setq user-full-name "Vincent Bernat")

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "<insert>"))
(define-key ctl-x-map "p" nil)
(after! project
  (map! "C-x p" #'+popup/other))
(map! "C-s" #'+default/search-buffer)
(map! "M-RET" #'electric-indent-just-newline)
(map! :map goto-map "j" #'avy-goto-subword-1)
(map! :map goto-map "M-j" #'avy-goto-subword-1)
(map! "C->" #'mc/mark-next-like-this)
(map! "C-<" #'mc/mark-previous-like-this)

(after! doom-modeline
  ;; Don't display percent position
  (setq doom-modeline-percent-position nil)
  ;; Remove size and column
  (remove-hook! doom-modeline-mode #'size-indication-mode)
  ;; Don't handle focus change (this is buggy)
  (remove-function after-focus-change-function #'doom-modeline-focus-change))

;; Magit!
(after! magit
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

  ;; Use M-x magit-describe-section-briefly to get a section name
  (setq magit-section-initial-visibility-alist
        '((stashes . hide)
          (unpushed . hide)
          (recent . show)
          (untracked . show)
          (unstaged . show)))
  (setq +magit-open-windows-in-direction 'down))
(after! git-commit
  (setq git-commit-summary-max-length 70))

;; Company mode
(after! company
  ;; Trigger company-mode explicitely
  (setq company-idle-delay nil)
  (map!
   :map company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common
   :map company-active-map "<tab>" #'company-complete-selection))

;; Format
(after! apheleia
  (setf (alist-get 'gofmt apheleia-formatters)
        '("goimports"))
  (setf (alist-get 'nixfmt apheleia-formatters)
        '("nixpkgs-fmt")))

;; dtrt-indent
(after! dtrt-indent
  (setq dtrt-indent-max-merge-deviation 10.0))

;; Flycheck
(after! flycheck
  (setq flycheck-temp-prefix ".flycheck")
  (after! python
    (add-hook! 'python-mode-hook
      (defun vbe:python-flychecker-setup ()
        (setq flycheck-flake8-maximum-line-length 88) ; match black
        (pushnew! flycheck-disabled-checkers 'python-pylint)))))

;; Projectile
(after! projectile
  (setq
   ;; Expire file cache quickly
   projectile-files-cache-expire 10)
  (pushnew! projectile-project-root-files "go.mod" "flakes.nix"))

;; Bookmarks
(map! :leader
      (:prefix ("b" . "bookmarks")
       :desc "Toggle"   "." #'bm-toggle
       :desc "Next"     "n" #'bm-next
       :desc "Previous" "p" #'bm-previous))
(after! bm
  (setq bm-cycle-all-buffers t))

;; Org
(setq org-directory "~/Documents/org/")
(after! org
  (setq org-log-done t
        org-log-into-drawer t
        org-image-actual-width (when window-system (list (truncate (* (frame-native-width) 0.9)))))
  (add-hook! org-mode (electric-indent-local-mode -1))
  (add-to-list 'org-structure-template-alist
               '("n" . "notes")))
(after! org-export
  (setq org-export-with-toc nil))
(after! org-clock
  (setq org-clock-mode-line-total 'current))
(after! org-download
  (setq org-download-image-dir "images"
        org-download-heading-lvl nil))
(after! org-html
  (setq org-html-postamble nil))

;; Tweak LSP to only try import if called interactively or an already known
;; workspace.
(when (fboundp 'lsp!)
  (defun lsp! ()
    "Dispatch to call the currently used lsp client entrypoint"
    (interactive)
    (if (modulep! :tools lsp +eglot)
        (eglot-ensure)
      (unless (bound-and-true-p lsp-mode)
        (require 'lsp)
        (when (or (lsp-workspace-root) (called-interactively-p 'any))
          (lsp-deferred))))))
(setq lsp-use-plists t)
(after! lsp-mode
  (map! :map lsp-help-mode-map "o" #'lsp--help-open-link)
  (setq
   lsp-modeline-diagnostics-enable nil
   lsp-modeline-code-actions-enable nil)
  (setq lsp-enable-suggest-server-download nil))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line))

;; Go mode
(after! go-mode
  (add-hook! go-mode #'apheleia-mode))
(after! go-guru
  (unless (executable-find go-guru-command)
    (when (executable-find "golang-guru")
      (setq go-guru-command "golang-guru"))))

;; JS/TS mode
(defadvice! vbe:javascript-init-lsp ()
  "Start LSP for the current buffer."
  :override #'+javascript-init-lsp-or-tide-maybe-h
  (lsp!))
(after! typescript-mode
  (setq typescript-indent-level 2))
(after! web-mode
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-script-padding 0
   web-mode-style-padding 0
   web-mode-enable-auto-indentation nil))

;; C
(after! cc-mode
  (require 'openbsd-knf-style)
  (c-add-style "openbsd" openbsd-knf-style))

;; Markdown
(after! markdown-mode
  (setq
   markdown-footnote-location 'immediately
   markdown-reference-location 'end
   markdown-spaces-after-code-fence 0))

;; Debian stuff
(after! dpkg-dev-el
  (setq debian-changelog-mailing-address (mapconcat 'identity '("bernat" "debian.org")  "@")))
