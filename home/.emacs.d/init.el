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
(setq package-dir
      (expand-file-name "packages" ttt-emacs-config-dir))

;; Set up load path
(add-to-list 'load-path ttt-lisp-dir)
(add-to-list 'load-path site-lisp-dir)
(let ((default-directory  package-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" ttt-lisp-dir))
(load custom-file)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ;;(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(exec-path-from-shell-initialize)

(require 'appearance)
(require 'editing)
(require 'minor_modes)
(require 'google_conf nil 'noerror)

; (eval-after-load 'EXAMPLE-mode '(require 'setup-EXAMPLE))
(eval-after-load 'tex '(require 'setup-latex))
(require 'setup-go)
(require 'setup-c)


(defun reload ()
  (interactive)
  (load ttt-emacs-init-file))
