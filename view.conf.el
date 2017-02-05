;;; Code:

;; The following function can be used with:
;;  emacsclient -t -e "(view-buffer (find-file-noselect \"CMakeLists.txt\") 'vbe:kill-buffer-and-frame)"

(defun vbe:kill-buffer-and-frame (buffer)
  "Kill BUFFER and the associated frame."
  (kill-buffer-if-not-modified buffer)
  (delete-frame))

;;; End
