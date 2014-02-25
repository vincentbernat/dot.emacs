(defun gnus-user-format-function-@ (header)
  "Display @ for message with attachment in summary line.
You need to add `Content-Type' to `nnmail-extra-headers' and
`gnus-extra-headers', see Info node `(gnus)To From Newsgroups'."
  (let ((case-fold-search t)
        (ctype (or (cdr (assq 'Content-Type (mail-header-extra header)))
                   "text/plain"))
        (indicator " "))
    (when (string-match "^multipart/mixed" ctype)
      (setq indicator "@"))
    indicator))

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yest, %H:%M")
        (604800 . "%a %H:%M") ;;that's one week
        ((gnus-seconds-month) . "%a %d")
        ((gnus-seconds-year) . "%b %d")
        ((* 30 (gnus-seconds-year)) . "%b %d '%y")
        (t . "")))

(copy-face 'default 'vbe:proportional)
(set-face-attribute 'vbe:proportional nil :font "DejaVu Sans-10")
(setq gnus-face-9  'font-lock-warning-face
      gnus-face-10 'shadow
      gnus-face-11 'vbe:proportional
      gnus-summary-line-format
      (concat
       "%10{%U%R%z%}" " " "%1{%11,11&user-date;%}"
       "%10{│%}"
       "%9{%u&@;%}" "%(%-15,15f %)"
       "%*"
       " " "%10{%B%}"
       "%11{%s%}\n"))

(setq
 gnus-summary-to-prefix "→ "
 gnus-sum-thread-tree-single-indent   "◎ "
 gnus-sum-thread-tree-false-root      "◌ "
 gnus-sum-thread-tree-root            "┌ "
 gnus-sum-thread-tree-vertical        "│"
 gnus-sum-thread-tree-leaf-with-other "├─►"
 gnus-sum-thread-tree-single-leaf     "╰─►"
 gnus-sum-thread-tree-indent          "  "
 gnus-summary-newsgroup-prefix "⇶"
 ;; Marks
 gnus-ticked-mark ?⚑
 gnus-dormant-mark ?⚐
 gnus-expirable-mark ?♻
 gnus-read-mark ?✓
 gnus-del-mark ?✗
 gnus-killed-mark ?☠
 gnus-replied-mark ?⟲
 gnus-forwarded-mark ?⤳
 gnus-cached-mark ?☍
 gnus-recent-mark ?★
 gnus-unseen-mark ?✩
 gnus-unread-mark ?✉
 gnus-score-over-mark ?↑           ; ↑ ☀
 gnus-score-below-mark ?↓)         ; ↓ ☂
;; Reevaluate gnus-auto-expirable-marks with those new symbols
(setq gnus-auto-expirable-marks (eval (car (get 'gnus-auto-expirable-marks 'standard-value))))

;; Group line format. Mostly stolen from Julien Danjou
(setq gnus-group-line-format "%ue%uM %S%p %P%5y:%B%(%g%)%O\n"
      gnus-topic-line-format "%i〜 %(%{%n%}%) 〜  %v\n")


(defun gnus-user-format-function-e (dummy)
  (vbe:gnus-image-or-space (char-to-string gnus-unread-mark)
			   (expand-file-name "icons/email.png" user-emacs-directory)
			   (> (string-to-number gnus-tmp-number-of-unread) 0)))
(defun gnus-user-format-function-M (dummy)
  (vbe:gnus-image-or-space (char-to-string gnus-ticked-mark)
			   (expand-file-name "icons/important.png" user-emacs-directory)
                          (cdr (assq 'tick gnus-tmp-marked))))

(defun vbe:gnus-image-or-space (string image image-p)
  (let ((image (create-image image)))
    (if (display-images-p)
	(if image-p
	    (propertize string 'display
			(append image
				'(:ascent center)))
	  (propertize " " 'display `(space . (:width ,(car (image-size image))))))
      (if image-p string " "))))


(setq nnmail-extra-headers
      '(To Cc Newsgroups Content-Type Thread-Topic Thread-Index))

(setq gnus-thread-hide-subtree nil	; expand threads
      gnus-summary-make-false-root 'empty ; add an empty node when needing a root node
      gnus-summary-make-false-root-always nil ; but only if needed
      gnus-fetch-old-headers nil
      gnus-build-sparse-threads 'some	; fetch some messages to get better threads
      gnus-single-article-buffer t)	; no more than one buffer per article

; Start in topic mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Ability to reply on top (shame, shame)...
(defun vbe:gnus/wide-reply-on-top (n)
  "Wide reply on top of the current message"
  (interactive "P")
  (vbe:gnus/reply-somehow-on-top n 'gnus-summary-wide-reply-with-original))
(defun vbe:gnus/reply-on-top (n)
  "Reply on top of the current message"
  (interactive "P")
  (vbe:gnus/reply-somehow-on-top n 'gnus-summary-reply-with-original))

(defun vbe:gnus/reply-somehow-on-top (n how)
  "Reply using HOW on top of the current message"
  (flet ((escape (s) (if (and s (string-match "%" s))
                         (mapconcat (lambda (c)
                                      (if (eq c ?%)
                                          "%%"
                                        (char-to-string c)))
                                    s "")
                       s)))
    (let ((message-cite-reply-position 'above)
          (message-citation-line-format
           (mapconcat 'identity
                      (delq nil
                            `(" ――――――― Original Message ―――――――"
                              " From: %f"
                              " Sent: %e %B %Y %R %Z"
                              ,(escape (concat " Subject: " (gnus-with-article-headers
                                                              (mail-fetch-field "Subject"))))
                              ,(escape (vbe:gnus/extract-names "To"))
                              ,(escape (vbe:gnus/extract-names "Cc"))
                              ""))
                      "\n")))
    (funcall how n))))

(define-key gnus-summary-mode-map (kbd "f") 'vbe:gnus/wide-reply-on-top)
(define-key gnus-summary-mode-map (kbd "r") 'vbe:gnus/reply-on-top)

(defun vbe:gnus/extract-names (field)
  "Extract the list of names from a given field"
  (let ((result (gnus-with-article-headers
	    (mapconcat '(lambda (x) (or (first x)
					(second x)))
		       (mail-extract-address-components
			(or (mail-fetch-field field)
			    "") t) "; "))))
    (when (> (length result) 0)
      (concat " " field ": "
	      (with-temp-buffer
		(insert result)
		(fill-region (point-min) (point-max))
		(replace-string "\n" (concat "\n"
					     (make-string (+ 3 (length field))
							  ? ))
				nil (point-min) (point-max))
		(buffer-substring (point-min) (point-max)))))))

