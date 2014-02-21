;; Deal with this early
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq ttt-emacs-init-file (or load-file-name buffer-file-name))
(setq ttt-emacs-config-dir (file-name-directory ttt-emacs-init-file))
(setq user-emacs-directory ttt-emacs-config-dir)
(setq ttt-lisp-dir (expand-file-name "ttt-lisp" ttt-emacs-config-dir))
(setq site-lisp-dir
      (expand-file-name "site-lisp" ttt-emacs-config-dir))

;; Set up load path
(add-to-list 'load-path ttt-lisp-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" ttt-lisp-dir))
(load custom-file)

(require 'appearance)
(require 'editing)
(require 'minor_modes)

;; Configurations
; (eval-after-load 'EXAMPLE-mode '(require 'setup-EXAMPLE))
(eval-after-load 'tex '(require 'setup-latex))

(defun reload ()
  (interactive)
  (load ttt-emacs-init-file))
