
;;;; alda-mode for emacs. A simple major mode that provides language features for alda

;;; Regexes
(let
  ;; Prevent regexes from taking up memory
  ((alda-comment-regexp "#.*$")
    (alda-instrument-regexp "^\\([a-zA-Z]\\{2\\}[A-Za-z0-9_\-]*\\)\\(\s\\(\"[A-Za-z0-9_\-]*\"\\)\\)?:")
    (alda-voice-regexp "\\([Vv][0-9]+:\\)")
    (alda-string-regexp "“\\([^ ]+?\\)”")
    (alda-timing-regexp "[a-gr]\\([~.0-9/]*\\)")
    (alda-pitch-shift "[a-gr]\\([\-\+]+\\)")
    (alda-set-octave-regexp "\\(o[0-9]+\\)")
    )

  (setq alda-highlights
    `((,alda-comment-regexp . font-lock-comment-face)
       (,alda-voice-regexp . (0 font-lock-function-name-face))
       (,alda-instrument-regexp . (1 font-lock-function-name-face))
       (,alda-string-regexp . (1 font-lock-string-face))
       (,alda-set-octave-regexp . (1 font-lock-type-face))
       (,alda-timing-regexp . (1 font-lock-builtin-face))
       (,alda-pitch-shift . (1 font-lock-preprocessor-face))
       )))

(define-derived-mode alda-mode fundamental-mode
  ;; Set alda comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")

  ;; Set alda highlighting
  (setq font-lock-defaults '(alda-highlights))
  (setq mode-name "Alda"))

(provide 'alda-mode)
