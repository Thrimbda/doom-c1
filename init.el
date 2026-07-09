;;; init.el -*- lexical-binding: t; -*-

;; This profile keeps Doom focused on Org mode and the norang workflow in
;; bh-org.el.  Run `doom sync` after changing this file.

(doom! :completion
       (vertico +icons)

       :ui
       doom
       dashboard
       hl-todo
       (ligatures)
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       (window-select +numbers)
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       snippets

       :emacs
       (dired +icons)
       electric
       (ibuffer +icons)
       undo

       :os
       (:if IS-MAC macos)
       tty

       :lang
       emacs-lisp
       (org +crypt +pretty)
       sh

       :config
       (default +bindings +smartparens))
