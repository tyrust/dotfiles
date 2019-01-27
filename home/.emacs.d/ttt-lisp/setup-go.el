(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(require 'company)
(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (local-set-key "\M-." 'godef-jump)))

(provide 'setup-go)
