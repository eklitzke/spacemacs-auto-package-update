Add something like this to your `dotspacemacs/user-config` function:

```elisp
;; Automatically update packages once a day.
(use-package spacemacs-auto-package-update
 :load-path "~/code/spacemacs-auto-package-update"
 :config (spacemacs-auto-package-update/check-and-apply-update))
```
