;;; spacemacs-auto-package-update.el -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Evan Klitzke
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(require 'cl)

(defvar spacemacs-auto-package-update/age (* 7 86400)
  "Automatically apply updates if this many seconds have passed.")

(defvar spacemacs-auto-package-update/file
  "~/.emacs.d/private/last-package-update"
  "The mtime of this file is used to track the last update time.")

(defun spacemacs-auto-package-update~file-age (attr)
  "Get the mtime age of attr in seconds."
  (cl-flet ((secs (t) (time-convert t 'integer)))
    (let ((now (secs nil))
          (mtime (secs (file-attribute-modification-time attr))))
      (- now mtime))))

(defun spacemacs-auto-package-update/apply-updates (update-file)
  "Apply package updates and update mtime of update-file."
  (message "Updating spacemacs packages for spacemacs-auto-package-update")
  (configuration-layer/update-packages t)
  (with-temp-file update-file (save-buffer)))

(defun spacemacs-auto-package-update/check-and-apply-updates ()
  "Apply updates if the update file is too old (or doesn't exist)."
  (let* ((update-file (expand-file-name spacemacs-auto-package-update/file))
         (attr (file-attributes update-file))
         (age (if (null attr) spacemacs-auto-package-update/age
                (spacemacs-auto-package-update~file-age attr))))
    (when (>= age spacemacs-auto-package-update/age)
      (spacemacs-auto-package-update/apply-updates update-file))))

(provide 'spacemacs-auto-package-update)
;;; spacemacs-auto-package-update.el ends here
