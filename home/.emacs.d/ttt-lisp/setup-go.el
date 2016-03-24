(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

(provide 'setup-go)
