;;; editor/format/config.el -*- lexical-binding: t; -*-

(defvar +format-preserve-indentation t
  "If non-nil, the leading indentation is preserved when formatting the whole
buffer. This is particularly useful for partials.

Indentation is always preserved when formatting regions.")

(defvar +format-with-lsp t
  "If non-nil, format with LSP formatter if it's available.

This can be set buffer-locally with `setq-hook!' to disable LSP formatting in
select buffers.")

(defvaralias '+format-with 'apheleia-formatter
  "Set this to explicitly use a certain formatter for the current buffer.")


;;
;;; Bootstrap

(when (modulep! +onsave)
  (add-hook 'doom-first-file-hook #'apheleia-global-mode))


;;
;;; Hacks

(defadvice! +format--inhibit-reformat-on-prefix-arg-a (orig-fn &optional arg)
  "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
  :around #'save-buffer
  (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
    (funcall orig-fn)))

(add-hook! 'apheleia-post-format-hook
           ;; HACK `web-mode' doesn't update syntax highlighting after arbitrary buffer
           ;;      modifications, so we must trigger refontification manually.
           (defun +format--fix-web-mode-fontification-h ()
             (when (eq major-mode 'web-mode)
               (setq web-mode-fontification-off nil)
               (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
                 (save-excursion
                   (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end)))))
           (defun +format--refresh-git-gutter-h ()
             (+vc-gutter-init-maybe-h)))


;;
;;; Additional formatters

(after! apheleia
  ;; TODO html-tidy
  (cl-defun apheleia--indent-lisp-buffer
      (&key buffer scratch callback &allow-other-keys)
    "Format a Lisp BUFFER. Use SCRATCH as a temporary buffer and CALLBACK to
apply the transformation. For more implementation detail, see
`apheleia--run-formatter-function'."
    (with-current-buffer scratch
      (setq-local indent-line-function
                  (buffer-local-value 'indent-line-function buffer))
      (setq-local lisp-indent-function
                  (buffer-local-value 'lisp-indent-function buffer))
      (funcall (with-current-buffer buffer major-mode))
      (goto-char (point-min))
      (let ((inhibit-message t)
            (message-log-max nil))
        (indent-region (point-min) (point-max)))
      (funcall callback))))
