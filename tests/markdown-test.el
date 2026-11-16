;;; tests/markdown-test.el -*- lexical-binding: t; -*-
;;
;; Run with: doom test

(require 'ert)
(require 'doom-lib)
(require 'markdown-mode)

(let ((config (expand-file-name "../config.el"
                                (file-name-directory (or load-file-name
                                                         buffer-file-name)))))
  (with-temp-buffer
    (insert-file-contents config)
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (equal (seq-take form 2) '(after! markdown-mode))
              (eval form t))))
      (end-of-file nil))))

(defmacro vbe:with-markdown (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (gfm-mode)
     (font-lock-ensure)
     (goto-char (point-min))
     ,@body))

(defun vbe:pre-p (line)
  (save-excursion
    (goto-char (point-min))
    (search-forward line)
    (and (get-text-property (line-beginning-position) 'markdown-pre) t)))

(defconst vbe:footnote "\
Text with a note[^module].

[^module]:
    A module is a function:

    - `imports` lists modules;
    - `options` declares options.

        footnote code

    Last paragraph of the note.

Normal paragraph.

    real code
")

(defconst vbe:admonition "\
Intro paragraph.

!!! \"Update (2026-11)\"
    I added an additional filter for the \"NoJS\" tracker to exclude bots pulling the
    URL from the page. The following snippet checks if the query has `r` set to
    `NoJS`.

        ::caddy
        route /count {
          log_skip
        }

    More text.

Normal paragraph.

    real code
")

(ert-deftest vbe:markdown-footnote-content-not-pre ()
  (vbe:with-markdown vbe:footnote
    (should-not (vbe:pre-p "A module is"))
    (should-not (vbe:pre-p "`imports`"))
    (should-not (vbe:pre-p "Last paragraph"))
    (should (vbe:pre-p "footnote code"))
    (should (vbe:pre-p "real code"))))

(ert-deftest vbe:markdown-admonition-content-not-pre ()
  (dolist (header '("!!!" "???" "???+"))
    (vbe:with-markdown (replace-regexp-in-string "^!!!" header vbe:admonition t t)
      (should-not (vbe:pre-p "I added"))
      (should-not (vbe:pre-p "More text"))
      (should (vbe:pre-p "::caddy"))
      (should (vbe:pre-p "log_skip"))
      (should (vbe:pre-p "real code")))))

(ert-deftest vbe:markdown-indented-text-without-header-is-pre ()
  (vbe:with-markdown "Paragraph.\n\n  not a header\n\n    code\n"
    (should (vbe:pre-p "code"))))

(ert-deftest vbe:markdown-admonition-fill ()
  (dolist (header '("!!!" "???" "???+"))
    (vbe:with-markdown (replace-regexp-in-string "^!!!" header vbe:admonition t t)
      (let ((fill-column 70))
        (search-forward "additional")
        (fill-paragraph)
        (goto-char (point-min))
        (should (search-forward
                 (concat header " \"Update (2026-11)\"\n"
                         "    I added an additional filter for the \"NoJS\" tracker to exclude\n"
                         "    bots pulling the URL from the page.")
                 nil t))
        (should (search-forward "\n\n        ::caddy\n" nil t))))))

(ert-deftest vbe:markdown-attribute-list-fill ()
  (vbe:with-markdown "\
The images below show the rendered layout for the three main web engines. You
need JavaScript to animate the paragraph widths and check the effects of the
various features.
{.when-nojs}
"
    (let ((fill-column 80))
      (fill-paragraph)
      (should (string-suffix-p "features.\n{.when-nojs}\n" (buffer-string))))))

;;; markdown-test.el ends here
