(defun gnus-user-format-function-@ (header)
  "Display @ for message with attachment in summary line.
You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
  (let ((case-fold-search t)
        (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                   "text/plain"))
        indicator)
    (when (string-match "^multipart/mixed" ctype)
      (setq indicator "@"))
    (if indicator
        indicator
      " ")))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yest, %H:%M")
        (604800 . "%a %H:%M") ;;that's one week
        ((gnus-seconds-month) . "%a %d")
        ((gnus-seconds-year) . "%b %d")
        ((* 30 (gnus-seconds-year)) . "%b %d '%y")
        (t . "")))

(setq gnus-face-9  'font-lock-warning-face)
(setq gnus-face-10 'shadow)
(setq gnus-summary-line-format
      (concat
       "%*"
       "%0{%U%R%z%}" "%10{│%}" "%1{%11,11&user-date;%}"
       "%10{│%}"
       "%9{%u&@;%}" "%(%-15,15f %)"
       "%10{│%}" "%10{%B%}" "%s\n"))

(setq
 gnus-sum-thread-tree-single-indent   "◎ "
 gnus-sum-thread-tree-false-root      "  "
 gnus-sum-thread-tree-root            "┌ "
 gnus-sum-thread-tree-vertical        "│"
 gnus-sum-thread-tree-leaf-with-other "├─►"
 gnus-sum-thread-tree-single-leaf     "└─►"
 gnus-sum-thread-tree-indent          "  ")

(setq gnus-extra-headers
      '(To Cc Newsgroups Content-Type))

(setq gnus-thread-hide-subtree nil	; expand threads
      gnus-summary-make-false-root 'empty ; add an empty node when needing a root node
      gnus-summary-make-false-root-always nil ; but only if needed
      gnus-fetch-old-headers nil
      gnus-build-sparse-threads 'some	; fetch some messages to get better threads
      gnus-single-article-buffer t)	; no more than one buffer per article

; Start in topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(provide 'vbe/gnus/summary)
