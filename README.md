# company-marklogic

Company backend for MarkLogic functions.

Using the Company Mode for Emacs, it suggests completions for MarkLogic
functions, as you type, both for JavaScript and XQuery.

## Install

Copy the three ELisp files somewhere in your load path, make sure you installed
Company Mode itself (e.g. using `M-x list-packages`), and put the following in
your init file (AKA your `~/.emacs` file):

    ;; load modules
    (require 'company)
    (require 'company-marklogic)
    
    ;; add both backends to the Company list
    (add-to-list 'company-backends 'company-marklogic-sjs)
    (add-to-list 'company-backends 'company-marklogic-xqy)
    
    ;; always use Company in JavaScript and XQuery modes
    (add-hook 'js-mode-hook     'company-mode)
    (add-hook 'xquery-mode-hook 'company-mode)

If you want to cherry-pick the Company backends to activate in JavaScript and/or
XQuery modes, use something like the following instead, for the corresponding
hooks:

    (add-hook 'js-mode-hook (lambda ()
      (setq-local company-backends '((company-marklogic-sjs company-dabbrev-code)))))
    
    (add-hook 'xquery-mode-hook (lambda ()
      (setq-local company-backends '((company-marklogic-xqy company-dabbrev-code)))))

If you use `use-package`, you can also use the following:

    (use-package company
      :hook (emacs-lisp-mode js-mode xquery-mode))
    
    (use-package company-marklogic
      :config
      (add-to-list 'company-backends 'company-marklogic-sjs)
      (add-to-list 'company-backends 'company-marklogic-xqy))

## Screenshots

In JavaScript:

![JavaScript sample](doc/screenshot-sjs.png)

In XQuery:

![XQuery sample](doc/screenshot-xqy.png)

## Dev notes

See `tools/make-tables.xq` in order to generate the XQuery and JavaScript tables.

**TODO**: Add the docstring for each function (see `company-backends` docstring
for more info.)

**TODO**: Is there anything to do here to integrate smoothly with
[company-box](https://github.com/sebastiencs/company-box)?
