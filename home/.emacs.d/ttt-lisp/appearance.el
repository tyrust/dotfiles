;;; appearance.el
;;; Generic appearance settings.

;; scroll size
(setq next-screen-context-lines 10)

;; display line and col number of insertion point
(setq line-number-mode t)
(setq column-number-mode t)

;; More descriptive buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Themes
(add-to-list 'custom-theme-load-path
      (expand-file-name "themes/emacs-color-theme-solarized" ttt-emacs-config-dir))
(load-theme 'solarized-dark t)

(provide 'appearance)
