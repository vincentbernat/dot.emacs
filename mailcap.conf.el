;; Use xdg-open, always.
(setq mailcap-mime-data '(("application" (".*"
                                          (viewer . "xdg-open '%s'")))
                          ("image" (".*"
                                   (viewer . "xdg-open '%s'")))
                          ("text" ("html"
                                   (viewer . "xdg-open '%s'"))))
      mailcap-parsed-p t)

