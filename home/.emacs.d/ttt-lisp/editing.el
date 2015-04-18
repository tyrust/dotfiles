;;; editing.el
;;; Generic settings, keybinds, and functions for all editing modes.

;; prefer text mode to fundamental mode
(setq-default major-mode 'text-mode)

;; tab width
(setq tab-width 4)
(setq-default indent-tabs-mode nil) ; spaces, not tabs

;; give C-x C-m and C-c C-m functionality of M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; prefer backward-kill-word over backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; run current file, bind to <f8>
(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be PHP, Perl, Python, Ruby, javascript, Bash, ocaml, vb, elisp.
File suffix is used to determine what program to run."
(interactive)
  (let (suffixMap fName suffix progName cmdStr)

    ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "js")
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            )
          )

    (setq fName (buffer-file-name))
    (setq suffix (file-name-extension fName))
    (setq progName (cdr (assoc suffix suffixMap)))
    (setq cmdStr (concat progName " \""   fName "\""))

    (if (string-equal suffix "el") ; special case for emacs lisp
        (load-file fName)
      (if progName
        (progn
          (message "Running…")
          (shell-command cmdStr "*run-current-file output*" )
          )
        (message "No recognized program file suffix for this file.")
        )
)))
; bind it
(global-set-key (kbd "<f8>") 'run-current-file)

;; compilation
(global-set-key "\C-cw" 'compile)
(global-set-key "\C-ce" 'recompile)
(global-set-key "\M-n" 'next-error)
(global-set-key "\M-p" '(lambda () (interactive) (next-error -1)))
; just fucking compile
(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)
; auto-scroll, but stop at first error
(setq compilation-scroll-output 'first-error)

;; tab completion
(setq read-file-name-completion-ignore-case t)

;;; BEGIN backups and autosaves
;;; (from http://www-users.math.umd.edu/~halbert/getcontent.cgi?code+emacs)
;; redefining the make-backup-file-name function in order to get
;; backup files in ~/.backups/ rather than scattered around all over
;; the filesystem. Note that you must have a directory ~/.backups/
;; made.  This function looks first to see if that folder exists.  If
;; it does not the standard backup copy is made.
(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (require 'dired)
  (if (file-exists-p "~/.backups")
      (concat (expand-file-name "~/.backups/")
              (dired-replace-in-string "/" "!" file-name))
    (concat file-name "~")))

;; redefining the make-auto-save-file-name function in order to get
;; autosave files sent to a single directory.  Note that this function
;; looks first to determine if you have a ~/.autosaves/ directory.  If
;; you do not it proceeds with the standard auto-save procedure.
(defun make-auto-save-file-name ()
  "Return file name to use for auto-saves of current buffer.."
  (if buffer-file-name
      (if (file-exists-p "~/.autosaves/")
          (concat (expand-file-name "~/.autosaves/") "#"
                  (replace-regexp-in-string "/" "!" buffer-file-name)
                  "#")
         (concat
          (file-name-directory buffer-file-name)
          "#"
          (file-name-nondirectory buffer-file-name)
          "#"))
    (expand-file-name
     (concat "#%" (buffer-name) "#"))))
;;; END backups and autosaves

;; Desktop setup
(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)
(setq desktop-path '("~/.emacs.d/desktops"))
(setq desktop-dirname "~/.emacs.d/desktops")
(setq desktop-base-file-name ".emacs-desktop")

;; IDO
(require 'ido)
(ido-mode t)

;; Persp mode
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation of restoring window configuration
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; don't fuck up
(setq confirm-kill-emacs 'yes-or-no-p)

(provide 'editing)
