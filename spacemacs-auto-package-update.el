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

(defvar spacemacs-auto-package-update/age 86400
  "Age in seconds to wait before updating")

(defvar spacemacs-auto-package-update/file
  "~/.emacs.d/private/last-package-update"
  "The file to track update times using.")

(defun spacemacs-auto-package-update/do-update (update-file)
  "Do a package update and update the timestamp on the update fille."
  (message "Need to do automatic spacemacs package update")
  (configuration-layer/update-packages t)
  (with-temp-file update-file (save-buffer)))

(defun spacemacs-auto-package-update/check-and-apply-update ()
  "Apply an update if the file is too old or doesn't exist."
  (let ((update-file (expand-file-name spacemacs-auto-package-update/file)))
    (when (or (not (file-exists-p update-file))
              (let* ((now (time-convert nil 'integer))
                     (last-seen (time-convert (file-attribute-modification-time (file-attributes update-file)) 'integer))
                     (delta (- now last-seen)))
                (>= delta spacemacs-auto-package-update/age)))
      (spacemacs-auto-package-update/do-update update-file))))

(provide 'spacemacs-auto-package-update)
;;; spacemacs-auto-package-update.el ends here
