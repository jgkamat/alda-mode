
;;;; alda-mode for emacs. A simple major mode that provides language features for alda

(require 'asm-mode)

;;; Regexes
(let
  ;; Prevent regexes from taking up memory
  ((alda-comment-regexp "\\(#.*$\\)")
    (alda-instrument-regexp "\\([a-zA-Z]\\{2\\}[A-Za-z0-9_\-]*\\)\\(\s+\\(\"[A-Za-z0-9_\-]*\"\\)\\)?:")
    (alda-voice-regexp "\\([Vv][0-9]+\\):")
    (alda-string-regexp "“\\([^ ]+?\\)”")
    (alda-timing-regexp "[a-gr]\s*\\([~.0-9/]*\\)")
    (alda-pitch-shift "[a-gr]\s*\\([-+]+\\)")
    (alda-bar-regexp "\\(|\\)")
    (alda-set-octave-regexp "\\(o[0-9]+\\)")
    (alda-shift-octave-regexp "\\(>\\|<\\)")
    (alda-variable-regexp "\\((\\(\\(quant\\(ization\\)?\\)\\|\\(tempo\\)\\|\\(vol\\(ume\\)?\\)\\)!?\s+[0-9]+)\\)")
    (alda-markers-regexp "\\([@%][a-zA-Z]\\{2\\}[a-zA-Z0-9()+-]*\\)")
    )

  (setq alda-highlights
    `((,alda-comment-regexp . (1 font-lock-comment-face) )
       (,alda-bar-regexp . (1 font-lock-comment-face))
       (,alda-voice-regexp . (1 font-lock-function-name-face))
       (,alda-instrument-regexp . (1 font-lock-function-name-face))
       (,alda-string-regexp . (1 font-lock-string-face))
       (,alda-variable-regexp . (1 font-lock-variable-name-face))
       (,alda-set-octave-regexp . (1 font-lock-type-face))
       (,alda-shift-octave-regexp . (1 font-lock-type-face))
       (,alda-timing-regexp . (1 font-lock-builtin-face))
       (,alda-markers-regexp . (1 font-lock-builtin-face))
       (,alda-pitch-shift . (1 font-lock-preprocessor-face))
       )))

(define-derived-mode alda-mode fundamental-mode
  ;; Set alda comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  (setq-local indent-line-function 'asm-indent-line)

  ;; Set alda highlighting
  (setq font-lock-defaults '(alda-highlights))
  (setq mode-name "Alda"))

;; Open alda files in alda-mode
(add-to-list 'auto-mode-alist '("\\.alda\\'" . alda-mode))

(provide 'alda-mode)
