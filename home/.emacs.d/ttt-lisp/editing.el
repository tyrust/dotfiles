;;; editing.el
;;; Generic settings, keybinds, and functions for all editing modes.

;; prefer text mode to fundamental mode
(setq-default major-mode 'text-mode)

;; tab width
(setq tab-width 4)
(setq-default indent-tabs-mode nil) ; spaces, not tabs

;; column-width
(setq-default fill-column 80) ; til the day I die

;; give C-x C-m and C-c C-m functionality of M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; prefer backward-kill-word over backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-cr" 'ff-find-other-file)

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

;; Desktop setup - CURRENTLY DISABLED
(require 'desktop)
; FIXME: ENABLE HERE
(desktop-save-mode 0)
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
(ido-everywhere 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Persp mode
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    (declare (indent 1) (debug t))
    `(eval-after-load ,file '(progn ,@body))))

(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil) ;; switch off animation of restoring window configuration
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (add-hook 'after-init-hook
    #'(lambda () (persp-mode 1))))

(with-eval-after-load "persp-mode"
  (setq persp-set-ido-hooks t))

(setq persp-auto-resume-time 0)
(defun ttt-compilation-buffer-name (mode)
  (format "*%s:%s*" (downcase mode) (safe-persp-name (get-current-persp))))
(setq compilation-buffer-name-function 'ttt-compilation-buffer-name)

;;; thingatpt
(require 'thingatpt)

;; http://emacsredux.com/blog/2013/07/25/increment-and-decrement-integer-at-point/
(defun thing-at-point-goto-end-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip over optional sign
    (when (looking-at "[+-]")
      (forward-char 1))
    ;; Skip over digits
    (skip-chars-forward "[[:digit:]]")
    ;; Check for at least one digit
    (unless (looking-back "[[:digit:]]")
      (error "No integer here"))))
(put 'integer 'beginning-op 'thing-at-point-goto-end-of-integer)

(defun thing-at-point-goto-beginning-of-integer ()
  "Go to end of integer at point."
  (let ((inhibit-changing-match-data t))
    ;; Skip backward over digits
    (skip-chars-backward "[[:digit:]]")
    ;; Check for digits and optional sign
    (unless (looking-at "[+-]?[[:digit:]]")
      (error "No integer here"))
    ;; Skip backward over optional sign
    (when (looking-back "[+-]")
        (backward-char 1))))
(put 'integer 'beginning-op 'thing-at-point-goto-beginning-of-integer)

(defun thing-at-point-bounds-of-integer-at-point ()
  "Get boundaries of integer at point."
  (save-excursion
    (let (beg end)
      (thing-at-point-goto-beginning-of-integer)
      (setq beg (point))
      (thing-at-point-goto-end-of-integer)
      (setq end (point))
      (cons beg end))))
(put 'integer 'bounds-of-thing-at-point 'thing-at-point-bounds-of-integer-at-point)

(defun thing-at-point-integer-at-point ()
  "Get integer at point."
  (let ((bounds (bounds-of-thing-at-point 'integer)))
    (string-to-number (buffer-substring (car bounds) (cdr bounds)))))
(put 'integer 'thing-at-point 'thing-at-point-integer-at-point)

(defun increment-integer-at-point (&optional inc)
  "Increment integer at point by one.

With numeric prefix arg INC, increment the integer by INC amount."
  (interactive "p")
  (let ((inc (or inc 1))
        (n (thing-at-point 'integer))
        (bounds (bounds-of-thing-at-point 'integer)))
    (delete-region (car bounds) (cdr bounds))
    (insert (int-to-string (+ n inc)))))

(defun decrement-integer-at-point (&optional dec)
  "Decrement integer at point by one.

With numeric prefix arg DEC, decrement the integer by DEC amount."
  (interactive "p")
  (increment-integer-at-point (- (or dec 1))))

(global-set-key (kbd "C-c +") 'increment-integer-at-point)
(global-set-key (kbd "C-c -") 'decrement-integer-at-point)


;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; don't fuck up
(setq confirm-kill-emacs 'yes-or-no-p)

;; Tramp
(setq tramp-default-method "ssh")


;; autocomplete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay              0
      company-minimum-prefix-length   2
      company-show-numbers            t
      company-tooltip-limit           20
      company-dabbrev-downcase        nil
      )

(defun complete-or-indent ()
  (interactive)
  (if (company-manual-begin)
      (company-complete-common)
          (indent-according-to-mode)))
;(local-set-key (kbd "TAB") 'complete-or-indent)
;(local-set-key [tab] 'complete-or-indent)

;;; https://github.com/company-mode/company-mode/wiki/Switching-from-AC#setting-up-similar-popup-behavior
(defun my-company-visible-and-explicit-action-p ()
  (and (company-tooltip-visible-p)
       (company-explicit-action-p)))

(defun company-ac-setup ()
  "Sets up `company-mode' to behave similarly to `auto-complete-mode'."
  (setq company-require-match nil)
  (setq company-auto-complete #'my-company-visible-and-explicit-action-p)
  (setq company-frontends '(company-echo-metadata-frontend
                            company-pseudo-tooltip-unless-just-one-frontend-with-delay
                            company-preview-frontend))
  (define-key company-active-map [tab] 'complete-or-indent)
  ;  'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'complete-or-indent)
  ;  'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(eval-after-load 'company
  (lambda ()
    (set-face-attribute
     'company-preview
     nil
     :background (face-attribute 'company-preview-common :background))))

(company-ac-setup)

(require 'ycmd)
(require 'company-ycmd)
(require 'flycheck-ycmd)

(company-ycmd-setup)
(flycheck-ycmd-setup)

;(add-hook 'after-init-hook #'global-ycmd-mode)
(add-hook 'c++-mode-hook 'ycmd-mode)
;(add-hook 'python-mode-hook 'ycmd-mode)
(set-variable 'ycmd-server-command '("python" "/home/tyrus/src/ycmd/ycmd"))
(set-variable 'ycmd-extra-conf-whitelist '("~/src/*"))
;(set-variable 'ycmd-request-msg-level -1)

(elpy-enable)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
;(add-to-list 'python-shell-completion-native-disabled-interpreters
;             "jupyter")

(provide 'editing)
