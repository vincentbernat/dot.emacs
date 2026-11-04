;;; cli.el -*- lexical-binding: t; -*-

;;;###autoload
(defun vbe:run-tests ()
  "Run the tests in `tests/'. Return nil if a test fails."
  (doom-initialize-packages)
  (let ((process-environment
         (cons (concat "EMACSLOADPATH=" (string-join (seq-filter #'file-directory-p load-path)
                                          path-separator))
               process-environment))
        (ok t))
    (dolist (file (doom-glob doom-user-dir "tests/*-test.el") ok)
      (print! (start "Running %s...") (file-name-nondirectory file))
      (unless (zerop (car (doom-exec-process
                           (expand-file-name invocation-name invocation-directory)
                           "-Q" "--batch" "-l" file
                           "-f" "ert-run-tests-batch-and-exit")))
        (setq ok nil)))))

;;;###autoload
(add-hook 'doom-after-sync-hook #'vbe:run-tests)
