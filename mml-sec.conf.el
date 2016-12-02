;; Quick fix for GPG key with DoB (see 25092)
(defun mml-secure-check-user-id (key recipient)
  "Check whether KEY has a non-revoked, non-expired UID for RECIPIENT."
  ;; Based on mml2015-epg-check-user-id.
  (let ((uids (epg-key-user-id-list key)))
    (catch 'break
      (dolist (uid uids nil)
        (if (and (stringp (epg-user-id-string uid))
                 (mail-header-parse-address (epg-user-id-string uid))
                 (equal (downcase (car (mail-header-parse-address
                                        (epg-user-id-string uid))))
                        (downcase (car (mail-header-parse-address
                                        recipient))))
                 (not (memq (epg-user-id-validity uid)
                            '(revoked expired))))
            (throw 'break t))))))
