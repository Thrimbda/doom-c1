;;; init.el -*- lexical-binding: t; -*-

;; This profile keeps Doom focused on Org mode and the norang workflow in
;; bh-org.el.  Run `doom sync` after changing this file.

(doom! :completion
       (vertico +icons)

       :ui
       doom
       dashboard
       modeline
       (popup +defaults)

       :editor
       (evil +everywhere)

       :emacs
       dired
       ibuffer
       undo

       :os
       (:if IS-MAC macos)
       tty

       :lang
       emacs-lisp
       (org +crypt)
       sh

       :config
       (default +bindings))
