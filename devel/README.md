# Init file for testing frame-workflow

This is an Emacs initialization file to used run another Emacs instance for testing without breaking the currently running Emacs session.

## Usage

Add this directory to the configuration of [Chemacs](https://github.com/plexus/chemacs):

``` emacs-lisp
(("default" . ((user-emacs-directory . "~/.emacs.d")))
 ("fwf-testing" . ((user-emacs-directory . "~/github/frame-workflow/devel"))))
```

Start Emacs with the config:

``` shell
emacs --with-profile fwf-testing
```
