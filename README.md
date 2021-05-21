This is a small package to help keep Spacemacs packages up to date, by updating
Spacemacs packages (using the `configuration-layer/update-packages` function) if
the mtime of a particular file exceeds some age.

The intended usage is to put this in your `dotspacemacs/user-config` function,
and the check will happen when you start Emacs. I prefer this to a timer because
checking for and applying updates right when I start Emacs is not a big deal to
me, but having Emacs prompt me to restart to apply updates while I'm in the
middle of working on code is annoying.

## Usage

Add some code like this to the `dotspacemacs/user-config` function in your
`~/.spacemacs` file:

```elisp
;; Load the package and invoke the update check.
(use-package spacemacs-auto-package-update
 :load-path "~/path/to/spacemacs-auto-package-update"
 :config (spacemacs-auto-package-update/check-and-apply-update))
```

You can also customize `spacemacs-auto-package-update/age` with the number of
seconds that should pass before checking for updates (default value is 604800,
the number of seconds in one week) and `spacemacs-auto-package-update/file` for
the path of the file to use to track when the last update occurred (default
value is `~/.emacs.d/private/last-package-update`).
