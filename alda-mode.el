;;; alda-mode.el --- A simple major mode for the musical programming language Alda

;; Copyright (C) 2016 Jay Kamat
;; Author: Jay Kamat <github@jgkamat.33mail.com>
;; Version: 0.2
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

;;; Constants:

(defconst +alda-output-buffer+ "*alda-output*")
(defconst +alda-output-name+ "alda-playback")
(defconst +alda-comment-str+ "#")

;;; Code:

;;; -- Region playback functions --

(defun alda-play-text (text)
  "Plays the given text using alda play --code.
Argument TEXT the text to play from"
  ;; Append an infinite loop if we will start a server
  (let ((process-loop-str
          (if (string-match "[Ss]erver [Dd]own" (shell-command-to-string "alda status"))
            (progn (message "Alda server down, starting in Emacs.") "&&  while true; do; sleep 1; done")
            "")))
    (if (eq (length (shell-command-to-string "which alda")) 0)
      (message "Alda was not found on your $PATH.")
      (progn
        (start-process-shell-command +alda-output-name+ +alda-output-buffer+
          (concat "alda play --code '" text
            ;; Infinite loop when server is down, prevents emacs from killing the alda server.
            "'" process-loop-str))))))

(defun alda-play-region (start end)
  "Plays the current selection in alda.
Argument START The start of the selection to play from.
Argument END The end of the selection to play from."
  (interactive "r")

  (if (eq start end)
    (message "No mark was set!")
    (alda-play-text (buffer-substring-no-properties start end))))

;; If evil is found, make evil commands as well.
(eval-when-compile
  (unless (require 'evil nil 'noerror)
    ;; Evil must be sourced in order to define this macro
    (defmacro evil-define-operator (name &rest trash)
      ;; Define a dummy instead if not present.
      `(defun ,name () (interactive) (message "Evil was not present while compiling alda-mode. Recompile with evil installed!")))))

;; Macro will be expanded based on the above dummy/evil load
(evil-define-operator alda-evil-play-region (beg end type register yank-hanlder)
  "Plays the text from BEG to END"
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (alda-play-region beg end))

(defun alda-stop ()
  "Stops songs from playing, and cleans up idle alda runner processes.
Because alda runs in the background, the only way to do this is with alda restart as of now."
  (interactive)
  (shell-command "alda stop -y")
  (delete-process +alda-output-buffer+))

;;; -- Font Lock Regexes --
(let
  ;; Prevent regexes from taking up memory
  ((alda-comment-regexp "\\(#.*$\\)")
    (alda-instrument-regexp "\\([a-zA-Z]\\{2\\}[A-Za-z0-9_\-]*\\)\\(\s*\\(\"[A-Za-z0-9_\-]*\"\\)\\)?:")
    (alda-voice-regexp "\\([Vv][0-9]+\\):")
    (alda-string-regexp "“\\([^ ]+?\\)”")
    (alda-timing-regexp "[a-gA-GrR][\s+-]*\\([~.0-9\s/]*\\(m?s\\)?\\)")
    (alda-cramming-regexp "\\({\\|}\\)")
    (alda-accidental-regexp "\\([a-gA-GrR]\s*[-+]+\\)")
    (alda-bar-regexp "\\(|\\)")
    (alda-set-octave-regexp "\\(o[0-9]+\\)")
    (alda-shift-octave-regexp "\\(>\\|<\\)")
    (alda-variable-regexp "\\(([a-zA-Z-]+!?\s+\\(\\([0-9]+\\)\\|\\(\\[\\(:[a-zA-Z]+\s?\\)+\\]\\)\\))\\)")
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
       (,alda-cramming-regexp . (1 font-lock-builtin-face))
       (,alda-markers-regexp . (1 font-lock-builtin-face))
       (,alda-accidental-regexp . (1 font-lock-preprocessor-face)))))

;;; -- Indention code --

;; A duplicate of asm-mode.el with changes
;; changes were made to the naming convention and to how the labels are calculated.
(defun alda-indent-line ()
  "Auto-indent the current line."
  (interactive)
  (let* ((savep (point))
          (indent (condition-case nil
                    (save-excursion
                      (forward-line 0)
                      (skip-chars-forward " \t")
                      (if (>= (point) savep) (setq savep nil))
                      (max (alda-calculate-indentation) 0))
                    (error 0))))
    (if savep
      (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun indent-prev-level ()
  "Indent this line to the indention level of the previous non-whitespace line."
  (save-excursion
    (forward-line -1)
    (while (and
             (not (eq (point) (point-min))) ;; Point at start of bufffer
             ;; Point has a empty line
             (let ((match-str (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
               (or (string-match "^\\s-*$" match-str)) (eq 0 (length match-str))))
      (forward-line -1))
    (current-indentation)))


(defun alda-calculate-indentation ()
  "Calculates indentation for `alda-mode' code."
  (or
    ;; Flush labels to the left margin.
    (and (looking-at "[A-Za-z0-9\" -]+:\\s-*$") 0)
    ;; All comments indention are the previous line's indention.
    (and (looking-at +alda-comment-str+) (indent-prev-level))
    ;; The rest goes at the first tab stop.
    (or (indent-next-tab-stop 0))))

(defun alda-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (let ((labelp nil))
    (save-excursion
      (skip-chars-backward "A-Za-z\"\s\t")
      (if (setq labelp (bolp)) (delete-horizontal-space)))
    (call-interactively 'self-insert-command)
    (when labelp
      (delete-horizontal-space)
      (tab-to-tab-stop))))

;;; -- Alda Keymaps --
;; TODO determine standard keymap for alda-mode

(defvar alda-mode-map nil "Keymap for `alda-mode'.")
(when (not alda-mode-map) ; if it is not already defined

  ;; assign command to keys
  (setq alda-mode-map (make-sparse-keymap))
  (define-key alda-mode-map (kbd ":") 'alda-colon)

  (define-key alda-mode-map [menu-bar alda-mode] (cons "Alda" (make-sparse-keymap)))
  (define-key alda-mode-map [menu-bar alda-mode alda-colon]
    '(menu-item "Insert Colon" alda-colon
       :help "Insert a colon; if it follows a label, delete the label's indentation")))


;;; -- Alda Mode Definition --

;;;###autoload
(define-derived-mode alda-mode prog-mode
  "Alda"
  "A major mode for alda-lang, providing syntax highlighting and basic indention."

  ;; Set alda comments
  (setq comment-start +alda-comment-str+)
  (setq comment-padding " ")
  (setq comment-start-skip (concat +alda-comment-str+ "\\s-*"))
  (setq comment-multi-line (concat +alda-comment-str+ " "))
  ;; Comments should use the indention of the last line
  (setq comment-indent-function #'indent-prev-level)

  ;; Set custom mappings
  (use-local-map alda-mode-map)
  (setq indent-line-function 'alda-indent-line)

  ;; Set alda highlighting
  (setq font-lock-defaults '(alda-highlights)))



;; Open alda files in alda-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.alda\\'" . alda-mode))

(provide 'alda-mode)

;;; alda-mode.el ends here
