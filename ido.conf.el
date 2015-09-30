;; IDO mode everywhere
(require 'flx-ido)
(require 'ido-vertical-mode)
(require 'recentf)
(setq ido-enable-flex-matching t        ; IDO flex matching
      ido-save-directory-list-file (expand-file-name "run/bookmarks" user-emacs-directory)
      ido-everywhere t                  ; use IDO whenever possible
      ido-use-faces nil)                ; let flx-ido do this job
(ido-mode 1)
(ido-vertical-mode 1)
(flx-ido-mode 1)
