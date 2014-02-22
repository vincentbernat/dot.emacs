(require 'dash)

;; Use xdg-open, always.
(mailcap-parse-mailcaps)
(setq mailcap-mime-data
      (--map (cons (car it)
                   (--map (cons (car it)
                                (--map (let ((key (car it))
                                             (value (cdr it)))
                                         (if (and (eq key 'viewer)
                                                  (stringp value))
                                             '(viewer . "xdg-open '%s'")
                                           it))
                                       (cdr it)))
                          (cdr it)))
             mailcap-mime-data))
