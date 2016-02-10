# Alda Mode

The best way to edit your [alda](https://github.com/alda-lang/alda) files in Emacs!

Alda mode is a simple mode to provide syntax highlighting and other basic language features for [Alda, the music programming language](https://github.com/alda-lang/alda) written in Clojure.

## Installation

The easiest way to install alda-mode is through [melpa](http://melpa.org/#/getting-started). Simply `M-x package-install` `melpa`.

Once installed, place `(require 'alda-mode)` in your init.el.

For evil integration, [require evil before alda](https://github.com/jgkamat/alda-mode#evil-integration).

Alda files (`.alda` extension) should now have syntax highlighting via font-lock-mode when opened.

If melpa won't work for you, `M-x package-install-file` alda-mode.el from this repository instead.

## Usage

alda-mode can play segments of alda from within Emacs! Simply highlight a section of alda code and run:

`M-x alda-play-region`

This will take a while during the first call, which will start the alda server within Emacs (if not already on your system). `alda` will need to be on your path.

Keep in mind, the server will only see the selection you make, and this has consequences. For example, you must run a snippet of code defining an instrument before you hear anything (or defining a marker before using it). `alda-play-region` will use a running alda server if it finds one, but will start one within Emacs if not found. Support is currently only available for Linux/Mac for this command.

Please submit any bugs with this feature as issues to this repository.

## Evil Integration

Evil integration is automatically applied if you require evil before alda-mode:

```
(require 'evil)
(require 'alda-mode)
```

This will make a evil operator called `alda-evil-play-region`.

## Keybindings

alda-mode does not assign any keybindings by default. Here are some simple example keybinds:

```
(global-set-key (kbd "C-c a") 'alda-play-region) ;; Global alda-play-map
;; Requires evil integration
(define-key evil-motion-state-map "gp" 'alda-evil-play-region)

```

This will map `C-c a` to play a selected region, and `gp` to be an evil operator to do the same thing! (`gpp` will play the current line, `gp20j` plays 20 lines, etc).

## Contributing

If you find a bug or want to improve alda-mode, submit a PR! =)

## License

alda-mode is licensed under the GPLv3.
