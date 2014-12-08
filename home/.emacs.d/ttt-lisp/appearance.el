;;; appearance.el
;;; Generic appearance settings.
(add-to-list 'default-frame-alist
             '(font . "Ubuntu Mono-11"))

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
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
                (load-theme 'solarized-dark t))))
    (load-theme 'solarized-dark t))

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

;; If the buffer in question is already displayed in a frame, raise that frame.
(setq-default display-buffer-reuse-frames t)

(provide 'appearance)
