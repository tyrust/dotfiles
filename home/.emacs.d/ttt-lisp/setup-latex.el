(setq TeX-auto-save t) 
(setq TeX-parse-self t) 
(setq TeX-save-query nil)

(setq ispell-program-name "aspell") 
    ; could be ispell as well, depending on your preferences 
(setq ispell-dictionary "english") 
    ; this can obviously be set to any language your spell-checking program supports

(add-hook 'LaTeX-mode-hook 'flyspell-mode) 
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)

(setq TeX-view-program-selection 
      (quote (((output-dvi style-pstricks) "dvips and gv") 
              (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))))

(provide 'setup-latex)
