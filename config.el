;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Set font and theme
(setq doom-font (font-spec :family "Iosevka Term SS18" :size 11.0)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 11.0))
(setq doom-theme 'doom-vibrant)
;; Remove some items from doom dashboard menu
(setq +doom-dashboard-menu-sections
      (seq-filter (fn! (not (seq-contains-p '("Open org-agenda" "Jump to bookmark") (car %))))
                  +doom-dashboard-menu-sections))
;; Change splash image
(setq fancy-splash-image (concat doom-user-dir "doom-emacs.svg"))
;; Don't display line numbers
(setq display-line-numbers-type nil)
;; Kill at the beginning of a line should also remove the line
(setq kill-whole-line t)
;; Always indent with tab (Doom Emacs set it to nil)
(setq-default tab-always-indent t)

(setq user-full-name "Vincent Bernat")

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "<insert>"))
(define-key ctl-x-map "p" nil)
(map! "C-x p" #'+popup/other)
(map! "C-s" #'+default/search-buffer)
(map! "M-RET" #'electric-indent-just-newline)
(map! :map goto-map "j" #'avy-goto-subword-1)
(map! :map goto-map "M-j" #'avy-goto-subword-1)
(map! "C->" #'mc/mark-next-like-this)
(map! "C-<" #'mc/mark-previous-like-this)

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
  (map! :map company-active-map "<tab>" #'company-complete-selection)
  (dolist (key '("<up>" "<down>" "SPC" "<return>" "RET"))
    (unbind-key key company-active-map)))

;; Format-all
(after! format-all
  (setq +format-with-lsp nil)
  (advice-add 'format-all-buffer--with :around #'envrc-propagate-environment))

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
  (pushnew! projectile-project-root-files "go.mod" "flakes.nix"))

;; Edit indirect allows to edit a region into a separate buffer
(map! :leader :desc "Edit indirect region" "'" #'edit-indirect-region)
(after! edit-indirect
  (defvar vbe:edit-indirect--left-margin 0)
  (defun vbe:compute-left-margin (code)
    "Compute left margin of a string of CODE."
    (-min
     (-map #'(lambda (line) (length (car (s-match "^\\s-*" line))))
           (-remove 's-blank? (s-lines code)))))
  (defun vbe:after-indirect-edit-remove-left-margin ()
    "Remove left-margin and save it into a local variable."
    (let ((lm (vbe:compute-left-margin (buffer-substring (point-min) (point-max)))))
      (indent-rigidly (point-min) (point-max) (* -1 lm))
      (setq-local vbe:edit-indirect--left-margin lm)))
  (defun vbe:after-indirect-edit-restore-left-margin ()
    "Restore left-margin before commiting."
    (indent-rigidly (point-min) (point-max) vbe:edit-indirect--left-margin))
  (add-hook 'edit-indirect-after-creation-hook #'vbe:after-indirect-edit-remove-left-margin)
  (add-hook 'edit-indirect-before-commit-hook #'vbe:after-indirect-edit-restore-left-margin))

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
(after! lsp-mode
  (setq lsp-enable-suggest-server-download nil))

(after! highlight-indent-guides
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function #'highlight-indent-guides--bitmap-line))

;; Go mode
(after! go-mode
  (unless (executable-find go-guru-command)
    (when (executable-find "golang-guru")
      (setq go-goru-command "golang-guru")))
  (add-hook! go-mode #'+word-wrap-mode)
  (add-hook! go-mode #'format-all-mode))

;; Nix mode
(after! nix-mode
  (after! format-all
    (set-formatter! 'nixfmt "nixpkgs-fmt")
    (puthash 'nixfmt "nixpkgs-fmt" format-all--executable-table)))

;; JS/TS mode
(defadvice! vbe:javascript-init-lsp ()
  "Start LSP for the current buffer."
  :override #'+javascript-init-lsp-or-tide-maybe-h
  (lsp!))
(after! typescript-mode
  (setq typescript-indent-level 2))

;; C
(after! cc-mode
  (require 'openbsd-knf-style)
  (c-add-style "openbsd" openbsd-knf-style))

;; Debian stuff
(after! dpkg-dev-el
  (setq debian-changelog-mailing-address (mapconcat 'identity '("bernat" "debian.org")  "@")))
