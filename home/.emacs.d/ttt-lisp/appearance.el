;;; appearance.el
;;; Generic appearance settings.
(add-to-list 'default-frame-alist
             '(font . "Ubuntu Mono:pixelsize=14"))

;; scroll size
(setq next-screen-context-lines 10)

;; display line and col number of insertion point
(setq line-number-mode t)
(setq column-number-mode t)

;; More descriptive buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Themes
;; Solarized - https://github.com/sellout/emacs-color-theme-solarized
(add-to-list 'custom-theme-load-path
    (expand-file-name "themes/emacs-color-theme-solarized" ttt-emacs-config-dir))
(setq frame-background-mode 'dark)
(load-theme 'solarized t)

;; TODO: migrate to github.com/bbatsov/solarized-emacs, which is much more
;; maintained, but term doesn't work (see URL
;; `https://github.com/bbatsov/solarized-emacs/issues/18').

; Modeline - https://github.com/Malabarba/smart-mode-line
(setq sml/theme 'respectful)
(sml/setup)

(setq sml/replacer-regexp-list
      '(("^~/\\.homesick/repos/dotfiles/home/" ":df:")))
; rich-minority-mode configuration - show only select minor modes
(mapc (lambda (pair) (add-to-list 'rm-blacklist pair t))
      '(" company" " ycmd" " Server"))

;; highlight at character limits
(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for a string beyond width WIDTH
that uses 'font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(font-lock-add-keywords 'c++-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'java-mode (font-lock-width-keyword 100))
(font-lock-add-keywords 'js-mode (font-lock-width-keyword 80))
(font-lock-add-keywords 'python-mode (font-lock-width-keyword 80))

(setq show-trailing-whitespace t)

;; If the buffer in question is already displayed in a frame, raise that frame.
(setq-default display-buffer-reuse-frames t)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *messages* from the buffer.
;(setq-default message-log-max nil)
;(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; ansi-color for compilation
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(provide 'appearance)
