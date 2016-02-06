;;; alda-mode.el --- A simple major mode that provides language features for the musical programming language, Alda

;; Copyright (C) 2016 Jay Kamat
;; Author: Jay Kamat <github@jgkamat.33mail.com>
;; Version: 0.1
;; Keywords: alda, highlight
;; URL: http://github.com/jgkamat/alda-mode
;; Package-Requires: ((emacs "24.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides syntax highlighting and basic alda integration.
;; Activate font-lock-mode to use the syntax features, and run 'alda-play-region' to play song files

;;; Code:

;; asm mode's indention is used for alda.
(require 'asm-mode)

(defconst +alda-output-buffer+ "*alda-output*")
(defconst +alda-output-name+ "alda-playback")

(defun alda-play-region (start end)
  "Plays the current selection in alda.
Argument START The start of the selection to play from.
Argument END The end of the selection to play from."
  (interactive "r")
  ;; Append an infinite loop if we will start a server
  (let ((process-loop-str
          (if (string-match "[Ss]erver [Dd]own" (shell-command-to-string "alda status"))
            (progn (message "Alda server down, starting in Emacs.") "&&  while true; do; sleep 1; done")
            "")))
    (if (eq (length (shell-command-to-string "which alda")) 0)
      (message "Alda was not found on your $PATH.")
      (if (eq start end)
        (message "No mark was set!")
        (progn
          (start-process-shell-command +alda-output-name+ +alda-output-buffer+
            (concat "alda play --code '" (buffer-substring-no-properties start end)
              ;; Infinite loop when server is down, prevents emacs from killing the alda server.
              "'" process-loop-str)))))))

(defun alda-stop ()
  "Stops songs from playing, and cleans up idle alda runner processes.
Because alda runs in the background, the only way to do this is with alda restart as of now."
  (interactive)
  (shell-command "alda stop -y")
  (delete-process +alda-output-buffer+))

;;; Regexes
(let
  ;; Prevent regexes from taking up memory
  ((alda-comment-regexp "\\(#.*$\\)")
    (alda-instrument-regexp "\\([a-zA-Z]\\{2\\}[A-Za-z0-9_\-]*\\)\\(\s+\\(\"[A-Za-z0-9_\-]*\"\\)\\)?:")
    (alda-voice-regexp "\\([Vv][0-9]+\\):")
    (alda-string-regexp "“\\([^ ]+?\\)”")
    (alda-timing-regexp "[a-gr][\s+-]*\\([~.0-9\s/]*\\)")
    (alda-accidental-regexp "\\([a-gr]\s*[-+]+\\)")
    (alda-bar-regexp "\\(|\\)")
    (alda-set-octave-regexp "\\(o[0-9]+\\)")
    (alda-shift-octave-regexp "\\(>\\|<\\)")
    (alda-variable-regexp "\\((\\(\\(quant\\(ization\\)?\\)\\|\\(tempo\\)\\|\\(vol\\(ume\\)?\\)\\)!?\s+[0-9]+)\\)")
    (alda-markers-regexp "\\([@%][a-zA-Z]\\{2\\}[a-zA-Z0-9()+-]*\\)"))

  (defvar alda-highlights
    `((,alda-comment-regexp . (1 font-lock-comment-face) )
       (,alda-bar-regexp . (1 font-lock-comment-face))
       (,alda-voice-regexp . (1 font-lock-function-name-face))
       (,alda-instrument-regexp . (1 font-lock-type-face))
       (,alda-string-regexp . (1 font-lock-string-face))
       (,alda-variable-regexp . (1 font-lock-variable-name-face))
       (,alda-set-octave-regexp . (1 font-lock-constant-face))
       (,alda-shift-octave-regexp . (1 font-lock-constant-face))
       (,alda-timing-regexp . (1 font-lock-builtin-face))
       (,alda-markers-regexp . (1 font-lock-builtin-face))
       (,alda-accidental-regexp . (1 font-lock-preprocessor-face))
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

;;; alda-mode.el ends here
