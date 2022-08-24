;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Siyuan Wang"
      user-mail-address "c.one@thrimbda.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/OneDrive/cone/")
(setq org-log-done 'time)
(setq org-export-in-background t)
(setq org-catch-invisible-edits 'smart)
(setq org-use-sub-superscripts '{})
(add-hook! 'org-mode-hook #'+org-pretty-mode #'mixed-pitch-mode)

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(custom-set-faces!
  '(org-document-title :height 1.2))

(setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline)))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))

(setq org-export-headline-levels 5)

(defun org-export-filter-text-acronym (text backend _info)
  "Wrap suspected acronyms in acronyms-specific formatting.
Treat sequences of 2+ capital letters (optionally succeeded by \"s\") as an acronym.
Ignore if preceeded by \";\" (for manual prevention) or \"\\\" (for LaTeX commands).

TODO abstract backend implementations."
  (let ((base-backend
         (cond
          ((org-export-derived-backend-p backend 'latex) 'latex)
          ;; Markdown is derived from HTML, but we don't want to format it
          ((org-export-derived-backend-p backend 'md) nil)
          ((org-export-derived-backend-p backend 'html) 'html)))
        (case-fold-search nil))
    (when base-backend
      (replace-regexp-in-string
       "[;\\\\]?\\b[A-Z][A-Z]+s?[^A-Za-z]"
       (lambda (all-caps-str)
         (cond ((equal (aref all-caps-str 0) ?\\) all-caps-str)                ; don't format LaTeX commands
               ((equal (aref all-caps-str 0) ?\;) (substring all-caps-str 1))  ; just remove not-acronym indicator char ";"
               (t (let* ((trailing-s (equal (aref all-caps-str (- (length all-caps-str) 2)) ?s))
                         (acr (substring all-caps-str 0 (if trailing-s -2 -1)))
                         (final-char (substring all-caps-str -1))) ; needed to re-insert the [^A-Z] at the end
                    (pcase base-backend
                      ('latex (concat "\\acr{" (s-downcase acr) "}" (when trailing-s "\\acrs{}") final-char))
                      ('html (concat "<span class='acr'>" acr "</span>" (when trailing-s "<small>s</small>") final-char)))))))
       text t t))))

(after! ox
  (add-to-list 'org-export-filter-plain-text-functions
               #'org-export-filter-text-acronym)
  )

;; We won't use `org-export-filter-headline-functions' because it
;; passes (and formats) the entire section contents. That's no good.

(defun org-html-format-headline-acronymised (todo todo-type priority text tags info)
  "Like `org-html-format-headline-default-function', but with acronym formatting."
  (org-html-format-headline-default-function
   todo todo-type priority (org-export-filter-text-acronym text 'html info) tags info))
(setq org-html-format-headline-function #'org-html-format-headline-acronymised)

(defun org-latex-format-headline-acronymised (todo todo-type priority text tags info)
  "Like `org-latex-format-headline-default-function', but with acronym formatting."
  (org-latex-format-headline-default-function
   todo todo-type priority (org-latex-substitute-verb-with-texttt
                            (org-export-filter-text-acronym text 'latex info)) tags info))
(setq org-latex-format-headline-function #'org-latex-format-headline-acronymised)

(setq +file-templates-inhibit nil)

;; org export tweaks

(defadvice! +chinese--org-md-paragraph-a (args)
  "Join consecutive Chinese lines into a single long line without unwanted space
when exporting org-mode to md."
  :filter-args #'org-md-paragraph
  (cl-destructuring-bind (paragraph contents info) args
    (let* ((fix-regexp "[[:multibyte:]]")
           (mixed-fix-regexp "[[:multibyte:]\|[:alnum:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat "\\(" fix-regexp "\\)[[:blank:]]*\n[[:blank:]]*\\(" fix-regexp "\\)")
             "\\1\\2"
             contents))
           (mixed-fixed-contents
            (replace-regexp-in-string
             (concat "\\(" mixed-fix-regexp "\\)[[:blank:]]*\n[[:blank:]]*\\(" mixed-fix-regexp "\\)")
             "\\1 \\2"
             fixed-contents))
           )
      (list paragraph mixed-fixed-contents info))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Font related
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 10)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 20)
      doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font Mono" :size 10)
      doom-unicode-font (font-spec :family "等距更纱黑体 SC" :size 12)
      doom-serif-font (font-spec :family "FiraCode Nerd Font Mono" :size 10))

(defvar required-fonts '("FiraCode Nerd Font"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(add-hook! 'doom-init-ui-hookdis
        (run-at-time nil nil
                     (lambda ()
                       (message "%s missing the following fonts: %s"
                                (propertize "Warning!" 'face '(bold warning))
                                (mapconcat (lambda (font)
                                             (propertize font 'face 'font-lock-variable-name-face))
                                           ',missing-fonts
                                           ", "))
                       (sleep-for 0.5)))))
  ";; No missing fonts detected")

(setq-default fill-column 72)

;; org related
(after! org
  ;; (setq org-startup-indented nil)
  (setq org-modules '(org-habit ol-bibtex))
  (add-hook! 'org-mode-hook #'auto-fill-mode)
  (use-package! org-thrill)
  (use-package! org-books))

(after! org-books
  (setq org-books-file "~/OneDrive/books.org"))

;; jsonnet
(use-package! jsonnet-mode)

;; citre
(use-package citre
  :defer t
  :init
  (require 'citre-config)
  :config
  (defun prog-mode-citre-bindings()
    (local-set-key (kbd "C-M-]") 'citre-jump)
    (local-set-key (kbd "C-M-t") 'citre-jump-back)
    (local-set-key (kbd "C-M-p") 'citre-peek)
    (local-set-key (kbd "C-M-i") 'custom-citre-completion-at-point)
    )
  (add-hook 'go-mode-hook 'prog-mode-citre-bindings)
  ;; (add-hook 'c-mode-hook 'prog-mode-citre-bindings)
)

;; rime
(use-package! rime
  :config
  (defadvice! +rime--posframe-display-result-a (args)
    "给 `rime--posframe-display-result' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
    :filter-args #'rime--posframe-display-result
    (cl-destructuring-bind (result) args
      (let ((newresult (if (string-blank-p result)
                           result
                         (concat result "　"))))
        (list newresult))))
  :custom
  (rime-show-candidate 'posframe)
  (default-input-method "rime")
  )

; (after! rime
;   (when (eq system-type 'darwin)
;     (rime-librime-root "~/.emacs.d/librime/dist"))
;   ;; FIXME: find a way to detect if nixos
;   (when (eq system-type 'gnu/linux)
;     (setq rime-emacs-module-header-root (file-truename (concat (file-name-directory (directory-file-name invocation-directory)) "/include")))
;     (setq rime-librime-root (shell-command-to-string "nix eval --raw --impure --expr '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib librime)'"))
;     (setq rime-share-data-dir (concat (shell-command-to-string "nix eval --raw --impure --expr '(let pkgs = import <nixpkgs> {}; in with pkgs; lib.getLib brise)'") "/share/rime-data"))
;     )
;   )

;; max-size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (when doom-debug-p
;;   (require 'benchmark-init)
;;   (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

;; modeline tweaks
;; (after! doom-modeline
;;   (doom-modeline-def-modeline 'main
;;     '(bar)))

;; ebooks
(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Calibre"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (map! :map calibredb-show-mode-map
        :ne "?" #'calibredb-entry-dispatch
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "q" #'calibredb-entry-quit
        :ne "." #'calibredb-open-dired
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments)
  (map! :map calibredb-search-mode-map
        :ne [mouse-3] #'calibredb-search-mouse
        :ne "RET" #'calibredb-find-file
        :ne "?" #'calibredb-dispatch
        :ne "a" #'calibredb-add
        :ne "A" #'calibredb-add-dir
        :ne "c" #'calibredb-clone
        :ne "d" #'calibredb-remove
        :ne "D" #'calibredb-remove-marked-items
        :ne "j" #'calibredb-next-entry
        :ne "k" #'calibredb-previous-entry
        :ne "l" #'calibredb-virtual-library-list
        :ne "L" #'calibredb-library-list
        :ne "n" #'calibredb-virtual-library-next
        :ne "N" #'calibredb-library-next
        :ne "p" #'calibredb-virtual-library-previous
        :ne "P" #'calibredb-library-previous
        :ne "s" #'calibredb-set-metadata-dispatch
        :ne "S" #'calibredb-switch-library
        :ne "o" #'calibredb-find-file
        :ne "O" #'calibredb-find-file-other-frame
        :ne "v" #'calibredb-view
        :ne "V" #'calibredb-open-file-with-default-tool
        :ne "." #'calibredb-open-dired
        :ne "b" #'calibredb-catalog-bib-dispatch
        :ne "e" #'calibredb-export-dispatch
        :ne "r" #'calibredb-search-refresh-and-clear-filter
        :ne "R" #'calibredb-search-clear-filter
        :ne "q" #'calibredb-search-quit
        :ne "m" #'calibredb-mark-and-forward
        :ne "f" #'calibredb-toggle-favorite-at-point
        :ne "x" #'calibredb-toggle-archive-at-point
        :ne "h" #'calibredb-toggle-highlight-at-point
        :ne "u" #'calibredb-unmark-and-forward
        :ne "i" #'calibredb-edit-annotation
        :ne "DEL" #'calibredb-unmark-and-backward
        :ne [backtab] #'calibredb-toggle-view
        :ne [tab] #'calibredb-toggle-view-at-point
        :ne "M-n" #'calibredb-show-next-entry
        :ne "M-p" #'calibredb-show-previous-entry
        :ne "/" #'calibredb-search-live-filter
        :ne "M-t" #'calibredb-set-metadata--tags
        :ne "M-a" #'calibredb-set-metadata--author_sort
        :ne "M-A" #'calibredb-set-metadata--authors
        :ne "M-T" #'calibredb-set-metadata--title
        :ne "M-c" #'calibredb-set-metadata--comments))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (map! :map nov-mode-map
        :n "RET" #'nov-scroll-up)

  (defun doom-modeline-segment--nov-info ()
    (concat
     " "
     (propertize
      (cdr (assoc 'creator nov-metadata))
      'face 'doom-modeline-project-parent-dir)
     " "
     (cdr (assoc 'title nov-metadata))
     " "
     (propertize
      (format "%d/%d"
              (1+ nov-documents-index)
              (length nov-documents))
      'face 'doom-modeline-info)))

  (advice-add 'nov-render-title :override #'ignore)

  (defun +nov-mode-setup ()
    (face-remap-add-relative 'variable-pitch
                             :family "Merriweather"
                             :height 1.4
                             :width 'semi-expanded)
    (face-remap-add-relative 'default :height 1.3)
    (setq-local line-spacing 0.2
                next-screen-context-lines 4
                shr-use-colors nil)
    (require 'visual-fill-column nil t)
    (setq-local visual-fill-column-center-text t
                visual-fill-column-width 180
                nov-text-width 80
                )
    (visual-fill-column-mode 1)
    (hl-line-mode -1)

    (add-to-list '+lookup-definition-functions #'+lookup/dictionary-definition)

    (setq-local mode-line-format
                `((:eval
                   (doom-modeline-segment--workspace-name))
                  (:eval
                   (doom-modeline-segment--window-number))
                  (:eval
                   (doom-modeline-segment--nov-info))
                  ,(propertize
                    " %P "
                    'face 'doom-modeline-buffer-minor-mode)
                  ,(propertize
                    " "
                    'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                    'display `((space
                                :align-to
                                (- (+ right right-fringe right-margin)
                                   ,(* (let ((width (doom-modeline--font-width)))
                                         (or (and (= width 1) 1)
                                             (/ width (frame-char-width) 1.0)))
                                       (string-width
                                        (format-mode-line (cons "" '(:eval (doom-modeline-segment--major-mode))))))))))
                  (:eval (doom-modeline-segment--major-mode)))))

  (add-hook 'nov-mode-hook #'+nov-mode-setup))

;; title
(after! projectile
  (setq frame-title-format
        '("%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
  )

;; profile
;; config.el ends here
