
;;;; alda-mode for emacs. A simple major mode that provides language features for alda

(require 'asm-mode)

(defconst alda-output-buffer "*alda-output*")
(defconst alda-output-name "alda-playback")

(defun alda-play-reigion (start end)
  "Plays the current selection in alda"
  (interactive "r")
  (if (eq (length (shell-command-to-string "which alda")) 0)
    (message "Alda was not found on your $PATH.")
    (if (eq start end)
      (message "No mark was set!")
      (progn
        (start-process-shell-command alda-output-name alda-output-buffer
          (concat "alda play --code '" (buffer-substring-no-properties start end)
            ;; Infinite loop, prevents emacs from killing the alda server.
            ;; TODO: make this happen only when running the server.
            "' &&  while true; do; sleep 1; done"))))))
(defun alda-stop ()
  "Stops songs from playing, and cleans up idle alda runner processes
Because alda runs in the background, the only way to do this is with alda restart as of now."
  (interactive)
  (shell-command "alda stop -y")
  (delete-process alda-output-buffer))

;;; Regexes
(let
  ;; Prevent regexes from taking up memory
  ((alda-comment-regexp "\\(#.*$\\)")
    (alda-instrument-regexp "\\([a-zA-Z]\\{2\\}[A-Za-z0-9_\-]*\\)\\(\s+\\(\"[A-Za-z0-9_\-]*\"\\)\\)?:")
    (alda-voice-regexp "\\([Vv][0-9]+\\):")
    (alda-string-regexp "“\\([^ ]+?\\)”")
    (alda-timing-regexp "[a-gr][\s+-]*\\([~.0-9\s/]*\\)")
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
