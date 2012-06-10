(setq message-signature 'fortune)
(defconst fortune-program nil
  "*Program used to generate epigrams, default \"fortune\".")

(defvar fortune-switches (list "-e"
			       "50%" (expand-file-name "~/.sigs/kernelcookies")
			       "50%" (expand-file-name "~/.sigs/prog-style"))
  "*List of extra arguments when `fortune-program' is invoked")

(defun fortune (&optional long-p)
  "Generate a random epigram.
An optional prefix argument generates a long epigram.
The epigram is inserted at point if called interactively."
  (interactive "*P")
  (let ((fortune-buffer (generate-new-buffer " fortune"))
        (fortune-string "Have an adequate day."))
    (unwind-protect
        (save-excursion
          (set-buffer fortune-buffer)
          (apply 'call-process
                 (append (list (or fortune-program "fortune") nil t nil)
                         fortune-switches
                         (list (if long-p "-l" "-s"))))
          (skip-chars-backward "\n\t ")
          (setq fortune-string (buffer-substring (point-min) (point))))
      (kill-buffer fortune-buffer))
    (if (interactive-p)
        (insert fortune-string))
    fortune-string))

(provide 'vbe/gnus/signature)
