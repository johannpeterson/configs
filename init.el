;;; -*- lexical-binding: t -*-

;; init.el -- Johann Peterson's emacs init file

;; Author: Johann Peterson <johann.peterson@gmail.com>

;;; Commentary:
;;; * introduction:
;;; ** sources:
;; some emacs init files posted online:
;; https://gitlab.com/shilling.jake/emacsd/-/blob/master/config.org?ref_type=heads
;; https://github.com/KaratasFurkan/.emacs.d
;; https://sanemacs.com/sanemacs.el
;; https://lupan.pl/dotemacs/
;; https://github.com/radian-software/radian?tab=readme-ov-file
;; https://github.com/patrickt/emacs

;; help:
;; https://github.com/jwiegley/use-package
;; https://github.com/radian-software/straight.el

;; setting the frame size on startup:
;; https://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window

;; hard-to-find help about opening buffer list windows:
;; https://stackoverflow.com/questions/1231188/emacs-list-buffers-behavior

;;; ** keybindings:
;;; Key bindings defined in this init.el
;;; <f5>,<f6>   Solarized dark, light
;;; M-+         increase text size
;;; M--, M-=    decrease text size
;;; M-0         reset text size
;;; C-c i       edit this init.el
;;; C-c r       reload this init.el
;;; C-=         expand-region
;;; C-n, C-p    company-select-next, -previous
;;; M-<, M->    company-select-first, -last
;;; <tab>       tab-indent-or-complete in company mode
;;; C-j         yas-expand
;;; C-c s       ivy-yasnippet
;;; rustic:
;;; M-j         lsp-ui-imenu
;;; M-?         lsp-find-references
;;; C-c C-c l   flycheck-list-errors
;;; C-c C-c a   lsp-execute-code-action
;;; C-c C-c r   lsp-rename
;;; C-c C-c q   lsp-workspace-restart
;;; C-c C-c Q   lsp-workspace-shutdown
;;; C-c C-c s   lsp-rust-analyzer-status
;;; C-c C-c e   lsp-rust-analyzer-expand-macro
;;; C-c C-c d   dap-hydra
;;; C-c C-c h   lsp-ui-doc-glance
;;; julia:
;;; <C-RET>     my/julia-repl-send-cell
;;; <M-RET>     julia-repl-send-line
;;; <S-return>  julia-repl-send-buffer
;;; M-z         unbound

;;; ** todo:
;;; - [ ] use doom? https://github.com/doomemacs/doomemacs
;;; - [ ] fix haskell LSP
;;; - [ ] test julia mode
;;; - [ ] figure out Python debugging

;;; * Code:
;;; Code:

;;; ========================================================
;;; ** performance:
;;; Turn off garbage collection while loading this file,
;;; and while in minibuffer.
;;; https://github.com/KaratasFurkan/.emacs.d#performance-optimization

(setq gc-cons-threshold most-positive-fixnum)

(defconst my-1mb 1048576)
(defconst my-20mb 20971520)
(defconst my-30mb 31457280)
(defconst my-50mb 52428800)

(defun fk/defer-garbage-collection ()
  "Effectively turn off garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))

(defun fk/restore-garbage-collection ()
  "Restore garbage collection threshold."
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold my-30mb))))

(add-hook 'emacs-startup-hook 'fk/restore-garbage-collection)
(add-hook 'minibuffer-setup-hook 'fk/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook 'fk/restore-garbage-collection)

(setq read-process-output-max (* 3 my-1mb))  ;; lsp-mode's performance suggest

;;; ========================================================
;;; ** constants:

(defconst my/init (eval-when-compile (expand-file-name "init.el" user-emacs-directory)))
(defconst my/custom (eval-when-compile (expand-file-name "custom.el" user-emacs-directory)))
(defconst my/backups (eval-when-compile (expand-file-name "backups" user-emacs-directory)))

(setq user-full-name "Johann Peterson"
      user-mail-address "johann.peterson@gmail.com")

;;; ========================================================
;;; ** appearance & behavior:
(setq inhibit-splash-screen t)                       ; no splash screen
(setq visible-bell t)                                ; don't beep
(unbind-key "C-z")                                   ; stop accidentally suspending
(fset 'yes-or-no-p 'y-or-n-p)                        ; use short answers
(put 'scroll-left 'disabled nil)                     ; no scroll bar
(setq-default indent-tabs-mode nil)                  ; use spaces, not tabs
(setq fill-column 80)                                ; fill to 80 columns
(global-hl-line-mode 1)                              ; always highlight line
(show-paren-mode 1)                                  ; highlight parentheses
(setq-default truncate-lines 1)                      ; don't wrap lines
(column-number-mode t)                               ; show column number
(setq linum-format "%4d ")                           ; Line number format
(delete-selection-mode 1)                            ; Selected text will be overwritten when you start typing
(setq help-window-select t)                          ; Switch to help windows so they can be closed quickly.
(global-set-key (kbd "C-x C-b") 'ibuffer)            ; use IBuffer instead of list-buffers
(setq dired-kill-when-opening-new-dired-buffer t)    ; don't keep old dired buffers
(setq show-trailing-whitespace t)                    ; Show trailing whitespace in buffers.
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)               ; Delete trailing whitespace on save
(setq vc-follow-symlinks t)                          ; don't ask before following links to git-controlled files.

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

;;; *** backups & autosaves:

(setq backup-by-copying t
      backup-directory-alist `(("." . ,my/backups))
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 1
      version-control t
      create-lockfiles nil)

(setq auto-save-file-name-transforms `((".*" ,(file-name-as-directory my/backups) t)))

(global-auto-revert-mode t)               ; Auto-update buffer if file has changed on disk

;;; *** settings for window-system vs terminal:
(if (display-graphic-p)
    (progn
      (menu-bar-mode 1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (set-frame-font "Anonymous Pro-12")
      (set-frame-size (selected-frame) 120 70)
      )
  (progn
    (menu-bar-mode -1)
    (tool-bar-mode -1)
  ))


;;; *** Mac keyboard changes:
;; (setq mac-option-key-is-meta nil) ; use command as Meta
;; (setq mac-command-key-is-meta t)  ; use command as Meta
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier nil)

(cond ((eq system-type 'darwin)
       (setq mac-command-modifier 'meta
             mac-option-modifier 'alt
             mac-right-option-modifier 'super)
       (bind-key "M-+" 'text-scale-increase)
       (bind-key "M-=" 'text-scale-increase)
       (bind-key "M--" 'text-scale-decrease)
       ;; This is copied from
       ;; https://zzamboni.org/post/my-emacs-configuration-with-commentary/
       (defun my/text-scale-reset ()
         "Reset text-scale to 0."
         (interactive)
         (text-scale-set 0))
       (bind-key "M-0" 'my/text-scale-reset)))

;;; ========================================================
;;; ** Editing emacs configuration:
;;; C-i to edit the configuration file
;;; C-r to reload the configuration file

(defun my/open-config ()
  "Open configuration file."
  (interactive)
  (find-file my/init))

(global-set-key (kbd "C-c i") 'my/open-config)

(defun my/reload-config ()
  "Reload configuration files."
  (interactive)
  (load-file my/init))

(global-set-key (kbd "C-c r") 'my/reload-config)

;;; ========================================================
;;; ** useful functions:

(defalias 'qrr 'query-replace-regexp)

;;; ========================================================
;;; ** packages:
;;; *** straight:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq package-enable-at-startup nil)
(setq straight-use-package-by-default t)

;;; *** solarized theme:

(use-package solarized-theme
  :init (load-theme 'solarized-dark t)
  (global-set-key (kbd "<f5>")
                  (lambda ()
                    (interactive)
                    (load-theme 'solarized-dark t))
                  )
  (global-set-key (kbd "<f6>")
                  (lambda ()
                    (interactive)
                    (load-theme 'solarized-light t))
                  )
)

;;; *** modus themes:

;;; https://github.com/protesilaos/modus-themes
;;; (require-theme 'modus-themes)

;;; All customizations here
;; (setq modus-themes-bold-constructs t
;;       modus-themes-italic-constructs t)

;;; Load the theme of choice (built-in themes are always "safe" so they
;;; do not need the `no-require' argument of `load-theme').
;; (load-theme 'modus-vivendi)

;; (define-key global-map (kbd "<f5>") #'modus-themes-toggle)

;;; *** diminish:
;;; Limit the mode labels dispolayed in the mode line.
(use-package diminish)

;;; *** vterm:
(use-package vterm)

;;; exec-path-from-shell:
;;; Gets the PATH from the shell.
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

;;; ansi-color:
;;; This is necessary to clean up the ANSI terminal codes in the compilation mode buffers.
(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter))

;;; rainbow-delimiter:
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;;; expand-region:
(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;;; smart-mode-line:
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t
        theme 'respectful
        sml/shorten-directory t
        sml/shorten-modes t
        sml/name-width 50
        sml/mode-width 'full
)
  (sml/setup)
  )

;;; vertico:
;;; vertico - selection pop-up
;;; https://github.com/minad/vertico/wiki/Migrating-from-Selectrum-to-Vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode))

;;; marginalia:
;;; Enable rich annotations using the Marginalia package
;;; https://github.com/minad/marginalia
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;; *** lsp & other IDE niceties:
;;; lsp-mode
(use-package lsp-mode
  :ensure
  :hook ((c-mode c++-mode js-mode python-mode rust-mode julia-mode web-mode)
         . lsp)

  :commands lsp
  ;; :custom
  ;; ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; (lsp-eldoc-render-all t)
  ;; (lsp-idle-delay 0.6)
  ;; ;; This controls the overlays that display type and other hints inline. Enable
  ;; ;; / disable as you prefer. Will require a `lsp-workspace-restart' to have an
  ;; ;; effect on open projects.
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (lsp-rust-analyzer-display-chaining-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-display-parameter-hints nil)
  ;; (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;; lsp-ui
;;; https://emacs-lsp.github.io/lsp-ui/#intro
;; (use-package lsp-ui)
(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;;; flycheck
;;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;;; company
;;; https://company-mode.github.io/
(use-package company
  :ensure
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete)))

;; Some functions that I got with someone else's setup,
;; but that I don't understand and I'm not currently using.

;; (defun company-yasnippet-or-completion ()
;;   (interactive)
;;   (or (do-yas-expand)
;;       (company-complete-common)))

;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "::") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :defer nil
  :custom
  (yas-indent-line nil)
  (yas-inhibit-overlay-modification-protection t)
  :custom-face
  (yas-field-highlight-face ((t (:inherit region))))
  :bind*
  (:map yas-minor-mode-map
   ("C-TAB" . yas-expand)
   ("TAB" . nil)
   ("<tab>" . nil)
   :map yas-keymap
   ("TAB" . (lambda () (interactive) (company-abort) (yas-next-field)))
   ("<tab>" . (lambda () (interactive) (company-abort) (yas-next-field))))
  :hook
  (snippet-mode . (lambda () (setq-local require-final-newline nil)))
  :config
  (yas-global-mode))
(use-package yasnippet-snippets)
(use-package ivy-yasnippet
  :bind (:map yas-minor-mode-map
              ("C-c s" . ivy-yasnippet)))

(use-package magit)

(defun set-rust-devdocs ()
  (setq-local devdocs-current-docs '("rust")))
(defun set-python-devdocs ()
  (setq-local devdocs-current-docs '("python~3.13")))
(defun set-julia-devdocs ()
  (setq-local devdocs-current-docs '("julia~1.10")))
(defun set-haskell-devdocs ()
  (setq-local devdocs-current-docs '("haskell~9")))
(defun set-latex-devdocs ()
  (setq-local devdocs-current-docs '("latex")))

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup)
  :init
  (add-hook 'python-mode-hook 'set-python-devdocs)
  (add-hook 'julia-mode-hook 'set-julia-devdocs)
  (add-hook 'rustic-mode-hook 'set-rust-devdocs)            ; works
                                        ;  (add-hook 'haskell-mode-hook 'set-haskell-devdocs)
  (add-hook 'haskell-mode-hook
            (lambda () (message "Haskell mode")))
  (add-hook 'TeX-mode-hook 'set-latex-devdocs)            ; doesn't seem to be working
  )

;;; ========================================================
;;; ** modes:

;;; *** prog-mode:

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'global-company-mode)
(add-hook 'compilation-mode-hook 'visual-line-mode)

;;; ========================================================
;;; *** text mode:

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'visual-line-mode)

;;; ========================================================
;;; *** lisp:
;;; https://lispcookbook.github.io/cl-cookbook/emacs-ide.html

(setq inferior-lisp-program (executable-find "sbcl"))

;;; ========================================================
;;; *** haskell:

(use-package lsp-haskell
  :hook (haskell-mode . #'lsp-deferred))

;;; ========================================================
;;; *** rust:
;;; https://robert.kra.hn/posts/rust-emacs-setup/
;;; https://github.com/rust-lang/rust-mode

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  )
  ;;  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
;;  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;;; https://github.com/grafov/rust-playground
(use-package rust-playground :ensure)

(use-package toml-mode :ensure)

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t))
;;   (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; (when (executable-find "lldb-mi")
;;   (use-package dap-mode
;;     :ensure
;;     :config
;;     (dap-ui-mode)
;;     (dap-ui-controls-mode 1)
;;
;;     (require 'dap-lldb)
;;     (require 'dap-gdb-lldb)
;;     ;; installs .extension/vscode
;;     (dap-gdb-lldb-setup)
;;     (dap-register-debug-template
;;      "Rust::LLDB Run Configuration"
;;      (list :type "lldb"
;;            :request "launch"
;;            :name "LLDB::Run"
;;            :gdbpath "rust-lldb"
;;            ;; uncomment if lldb-mi is not in PATH
;;            ;; :lldbmipath "path/to/lldb-mi"
;;            ))))

;;; ========================================================
;;; *** julia:
;;; https://hershsingh.net/blog/emacs-julia/
;;; https://github.com/tpapp/julia-repl

(use-package julia-mode)
(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)

  :init
  (setenv "JULIA_NUM_THREADS" "8")

  :config
  ;; Set the terminal backend
  (julia-repl-set-terminal-backend 'ansi-term)

  ;; Keybindings for quickly sending code to the REPL
  (define-key julia-repl-mode-map (kbd "C-<return>") 'my/julia-repl-send-cell)
  (define-key julia-repl-mode-map (kbd "<M-RET>") 'julia-repl-send-line)
  (define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer))

(use-package lsp-julia
  :config
  ;; (setq lsp-julia-default-environment "~/.julia/environments/v1.6")
  :init
  (add-hook 'julia-mode-hook #'lsp-mode)
  )

(defun my/julia-repl-send-cell()
  ;; "Send the current julia cell (delimited by ###) to the julia shell"
  (interactive)
  (save-excursion (setq cell-begin (if (re-search-backward "^###" nil t) (point) (point-min))))
  (save-excursion (setq cell-end (if (re-search-forward "^###" nil t) (point) (point-max))))
  (set-mark cell-begin)
  (goto-char cell-end)
  (julia-repl-send-region-or-line)
  (next-line))

;;; ========================================================
;;; *** python:

(use-package python-mode
  :hook ((python-mode . (lambda ()
                          (when (require 'lsp-python nil t)
                            (lsp))))))

(use-package blacken
  :hook (python-mode . blacken-mode))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")

(setq gud-pdb-command-name "python3 -m pdb")

;;; ========================================================
;;; *** LaTeX:
;;; https://michaelneuper.com/posts/efficient-latex-editing-with-emacs/

(setq TeX-auto-save t
      TeX-parse-self t)

(use-package auctex)
(use-package pdf-tools)
;; (setq +latex-viewers '(pdf-tools))

(use-package company-auctex
  :init (company-auctex-init))

;; view generated PDF with `pdf-tools', not external viewer.
(unless (assoc "PDF Tools" TeX-view-program-list-builtin)
  (add-to-list 'TeX-view-program-list-builtin
               '("PDF Tools" TeX-pdf-tools-sync-view)))
(add-to-list 'TeX-view-program-selection
             '(output-pdf "PDF Tools"))

;;; ========================================================
;;; * custom:

(setq custom-file my/custom)
(load custom-file t)

;;; ** file settings:
;;; Local Variables:
;;; outline-regexp: ";;; \\*+"
;;; outline-heading-end-regexp: ":\n"
;;; eval: (outline-minor-mode 1)
;;; eval: (goto-address-mode 1)
;;; End:

;;; init.el ends here
