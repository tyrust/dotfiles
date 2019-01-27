; (setq-default c-basic-offset 4)

(require 'clang-format)
(local-set-key [C-M-tab] 'clang-format-region)
(defun clang-format-before-save ()
  "Based on gofmt-before-save"
  (interactive)
  (when (member major-mode '(c-mode c++-mode)) (clang-format-buffer)))
(add-hook 'before-save-hook 'clang-format-before-save)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; ;; replace the `completion-at-point' and `complete-symbol' bindings in
;; ;; irony-mode's buffers by irony-mode's function
;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))
;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (eval-after-load 'company
;;   '(add-to-list 'company-backends '(company-c-headers company-irony)))

;; ;; =============
;; ;; flycheck-mode
;; ;; =============
;; (add-hook
;;  'c++-mode-hook
;;  (lambda ()
;;    (setq flycheck-gcc-language-standard "c++14")
;;    'flycheck-mode))
;; (add-hook 'c-mode-hook 'flycheck-mode)
;; (eval-after-load 'flycheck
;; '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;; ;; =============
;; ;; eldoc-mode
;; ;; =============
;; (add-hook 'irony-mode-hook 'irony-eldoc)
;; ;; ==========================================
;; ;; (optional) bind TAB for indent-or-complete
;; ;; ==========================================
;; (defun irony--check-expansion ()
;; (save-excursion
;;   (if (looking-at "\\_>") t
;;     (backward-char 1)
;;     (if (looking-at "\\.") t
;;       (backward-char 1)
;;       (if (looking-at "->") t nil)))))
;; (defun irony--indent-or-complete ()
;; "Indent or Complete"
;; (interactive)
;; (cond ((and (not (use-region-p))
;;             (irony--check-expansion))
;;        (message "complete")
;;        (company-complete-common))
;;       (t
;;        (message "indent")
;;        (call-interactively 'c-indent-line-or-region))))
;; (defun irony-mode-keys ()
;; "Modify keymaps used by `irony-mode'."
;; (local-set-key (kbd "TAB") 'irony--indent-or-complete)
;; (local-set-key [tab] 'irony--indent-or-complete))
;; (add-hook 'c-mode-common-hook 'irony-mode-keys)

(provide 'setup-c)
