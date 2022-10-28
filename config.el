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

(setq user-full-name "Vincent Bernat")
(setq org-directory "~/Documents/org/")

(setq +format-on-save-enabled-modes
      '(go-mode))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "<insert>"))
(map! "C-s" #'+default/search-buffer)
(map! "M-RET" #'electric-indent-just-newline)
(map! :map goto-map "j" #'avy-goto-subword-1)
(map! :map goto-map "M-j" #'avy-goto-subword-1)

(use-package! magit
  :bind (("C-c g" . magit-file-dispatch))
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

  :custom
  ;; Use M-x magit-describe-section-briefly to get a section name
  (magit-section-initial-visibility-alist
   '((stashes . hide)
     (unpushed . hide)
     (recent . show)
     (untracked . show)
     (unstaged . show))))

;; Edit indirect allows to edit a region into a separate buffer
(use-package! edit-indirect
  :bind (("C-c '" . edit-indirect-region))
  :config
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
(use-package! bm
  :bind (("C-c C-." . bm-toggle)
         ("C-c C-/" . bm-next)
         ("C-c C-," . bm-previous))
  :custom
  (bm-cycle-all-buffers t))
