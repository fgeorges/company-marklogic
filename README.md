# company-ml

Company backend for MarkLogic functions.

Using the Company Mode for Emacs, it suggests completions for MarkLogic
functions, as you type, both for Javascript and XQuery.

## Install

Copy the three ELisp files somewhere in your load path, make sure you installed
Company Mode itself (e.g. using `M-x list-packages`), and put the following in
your init file (AKA your `~/.emacs` file):

    ;; load modules
    (require 'company)
    (require 'company-ml)
    
    ;; add both backends to the Company list
    (add-to-list 'company-backends 'company-ml-sjs)
    (add-to-list 'company-backends 'company-ml-xqy)
    
    ;; always use Company in Javascript and XQuery modes
    (add-hook 'js-mode-hook     'company-mode)
    (add-hook 'xquery-mode-hook 'company-mode)

If you want to cherry-pick the Company backends to activate in Javascript and/or
XQuery modes, use something like the following instead, for the corresponding
hooks:

    (add-hook 'js-mode-hook (lambda ()
      (setq-local company-backends '((company-ml-sjs company-dabbrev-code)))))
    
    (add-hook 'xquery-mode-hook (lambda ()
      (setq-local company-backends '((company-ml-xqy company-dabbrev-code)))))

If you use `use-package`, you can also use the following:

    (use-package company
      :hook (emacs-lisp-mode js-mode xquery-mode))
    
    (use-package company-ml
      :config
      (add-to-list 'company-backends 'company-ml-sjs)
      (add-to-list 'company-backends 'company-ml-xqy))


## Dev notes

See `tools/make-tables.xq` in order to generate the XQuery and Javascript tables.

**TODO**: Add the docstring for each function (see `company-backends` docstring
for more info.)
