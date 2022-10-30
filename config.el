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

(setq +format-on-save-enabled-modes
      '(go-mode))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "<insert>"))
(map! "C-s" #'+default/search-buffer)
(map! "M-RET" #'electric-indent-just-newline)
(map! :map goto-map "j" #'avy-goto-subword-1)
(map! :map goto-map "M-j" #'avy-goto-subword-1)

;; Magit!
(map! "C-c g" #'magit-file-dispatch)
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
          (unstaged . show))))

;; Company mode
(after! company
  ;; (map! :map company-active-map "<tab>" #'company-complete-selection)
  (dolist (key '("<up>" "<down>" "SPC" "<return>" "RET"))
    (unbind-key key company-active-map)))

;; Edit indirect allows to edit a region into a separate buffer
(map! "C-c '" #'edit-indirect-region)
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
(map! "C-c C-." #'bm-toggle)
(map! "C-c C-/" #'bm-next)
(map! "C-c C-," #'bm-prev)
(after! bm
  (setq bm-cycle-all-buffers t))

;; Org
(setq org-directory "~/Documents/org/")
(after! org
  (setq org-log-done t
        org-log-into-drawer t
        org-image-actual-width (when window-system (list (truncate (* (frame-native-width) 0.9)))))
  (add-hook! org-mode (electric-indent-local-mode -1))
  (add-hook! org-mode (when (and buffer-file-name
                                 (f-ancestor-of? org-directory buffer-file-name))
                        (git-auto-commit-mode 1)))
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
