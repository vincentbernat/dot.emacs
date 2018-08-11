;;; vbe-common.el --- Utility functions                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Vincent Bernat

;; Author: Vincent Bernat <bernat@luffy.cx>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'vbe-package)

;; Ensure the New Standard Library is installed and always available.
;; f.el    - files and paths  https://github.com/rejeep/f.el
;; s.el    - strings          https://github.com/magnars/s.el
;; dash.el - lists            https://github.com/magnars/dash.el
(use-package f)
(use-package s)
(use-package dash)
(use-package dash-functional)

;; Use `exec-path-from-shell' package to update PATH to the same value
;; as the shell.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(defun vbe:online? ()
  "Check if we are online.
This relies on NetworkManager and use D-Bus to open a connection
to it. It's also checking we are connected over wifi or
ethernet."
  (require 'dbus)
  (let ((ofNM "org.freedesktop.NetworkManager"))
    (when (and (member ofNM
                       (dbus-list-names :system))
               ;; Do we have a global connection?
               (eq (dbus-get-property :system
                                      ofNM "/org/freedesktop/NetworkManager"
                                      ofNM "State")
                   70))               ; 70 = NM_STATE_CONNECTED_GLOBAL
      ;; Is it metered?
      (-when-let (primary-connection
                  (dbus-get-property :system
                                     ofNM "/org/freedesktop/NetworkManager"
                                     ofNM "PrimaryConnection"))
        (--all? (memq (dbus-get-property :system
                                         ofNM it
                                         (concat ofNM ".Device") "Metered")
                      '(2               ; 2 = NM_METERED_NO
                        4))             ; 4 = NM_METERED_GUESS_NO
                 (dbus-get-property :system
                                    ofNM primary-connection
                                    (concat ofNM ".Connection.Active") "Devices"))))))

(defun vbe:runtime-directory (&rest names)
  "Return (and create) a directory for runtime files using NAMES."
  (apply 'f-mkdir user-emacs-directory "run" names)
  (apply 'f-join user-emacs-directory "run" names))
(defun vbe:runtime-file (&rest names)
  "Return the path for a runtime file.
The filename is the last NAMES provided while the remaining ones
are used for directory. The directory is created if needed."
  (f-join (apply 'vbe:runtime-directory (-drop-last 1 names))
          (-last-item names)))
(defun vbe:executable-path (name)
  "Return the path for executable NAME.
This only looks in ~/.emacs.d/bin!"
  (f-join user-emacs-directory "bin" name))

(defvar vbe:dpi-change-hook nil
  "Hook run when there is a DPI change.")
(defun vbe:after/font-setting-change-default-font (display-or-frame set-font)
  "Function to be executed after a setting change.
Both DISPLAY-OR-FRAME and SET-FONT are ignored."
  (run-hooks 'vbe:dpi-change-hook))
(advice-add 'font-setting-change-default-font
            :after #'vbe:after/font-setting-change-default-font)

;; The following function can be used with:
;;  emacsclient -t -e "(view-buffer (find-file-noselect \"CMakeLists.txt\") 'vbe:kill-buffer-and-frame)"
(defun vbe:kill-buffer-and-frame (buffer)
  "Kill BUFFER and the associated frame."
  (kill-buffer-if-not-modified buffer)
  (delete-frame))

(provide 'vbe-common)
;;; vbe-common.el ends here
