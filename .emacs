;;; .emacs -- Johann Peterson's emacs init file

;; Author: Johann Peterson <johann.peterson@gmail.com>

;;; Commentary:

;;; Code
                                        ;
;; Text mode and Auto Fill mode

;; The next two lines put Emacs into Text mode
;; and Auto Fill mode, and are for writers who
;; want to start writing prose rather than code.

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; General emacs configuration stuff

(setq mac-option-key-is-meta nil) ; use command as Meta
(setq mac-command-key-is-meta t)  ; use command as Meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(global-display-line-numbers-mode t)

;; --------------------------------------------------------
;; packages set-up stuff
;; --------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")))
;; (add-to-list 'package-archives
;;             (cons "melpa" "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; (defvar myPackages
;;   '(
;;     use-package
;;     pyenv
;;     elpy
;;     web-mode
;;     org
;;     org-latex
;;    )
;;   )

;; ;; Scans the list in myPackages
;; ;; If the package listed is not already installed, install it
;; (mapc #'(lambda (package)
;;           (unless (package-installed-p package)
;;             (package-install package)))
;;       myPackages)

; https://github.com/jwiegley/use-package
(require 'use-package)

;; put all the ~ backup files in .emacs.d/backups/
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; package exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized/")

(setq inhibit-splash-screen t)
;; only type `y` instead of `yes`
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 1)
(tool-bar-mode -1) ;; no toolbar

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters)

;; Keep rainbow-delimiters from changing the line height
(with-eval-after-load 'rainbow-delimiters
  (dotimes (i rainbow-delimiters-max-face-count)
    (set-face-attribute (intern (format "rainbow-delimiters-depth-%d-face" (1+ i)))
                        nil
                        :inherit 'default)))

(if window-system
    (progn
      (scroll-bar-mode -1)
      (set-frame-font "Anonymous Pro-12")))
(put 'scroll-left 'disabled nil)

;; Causes an error:
;; (load-theme 'zenburn t)

;; fuzzy matching on find-file, buffer switch
(ido-mode t)
(setq ido-enable-flex-matching t)
(add-to-list 'ido-ignore-files "\\.pyc")

;; (desktop-save-mode 1) ;; auto-save buffer state on close for a later time.
;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;; where to save auto-replace maps

;; ---------------------------------------------------------
;; LaTeX
;; ---------------------------------------------------------

(setq latex-run-command "pdflatex")

(add-hook 'latex-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'tex-mode-hook 'display-line-numbers-mode)

(setq reftex-plug-into-auctex t)

;; used for AucTeX?
;; (TeX-global-PDF-mode t)

;; ---------------------------------------------------------

(defalias 'qrr 'query-regexp-replace)
(setq-default indent-tabs-mode nil)
;; M-q should fill at 80 chars, not 75
(setq fill-column 80)
(global-hl-line-mode 1)
;; no wordwrap
(setq-default truncate-lines 1)
(line-number-mode t)
(column-number-mode t)

;; ---------------------------------------------------------

; Disabled - does not seem to work well with web-mode.
; fill-column
;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode
;;   global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)

;; ---------------------------------------------------------

;; web-mode
;; http://web-mode.org

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 0)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq indent-tabs-mode nil)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; ---------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "52fec4eebee0219de9b619c67ff72721fa8e462663b31e6573632e3e590ca900" "382956945efdbe0e39e06d6d7253f3bf05bdd98d2d82f1105dbe33b261338a46" "e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" "7e2b3b55e988a1828a11b706745c2d9b898879a0fb4fd9efdc5d86558c930b00" "6743c7238e9bd245782a680f92bee43261faf4997b66835c620fc64202b22663" "521e7ae4cbbbccc9f996d2292732b266afce23ef0de0e91236cc9629eb5f5a26" "76bb165fc9f375ec9f2308dabf1697e982f92ffd660a3cd933832da647df684d" "1838722404e4fe7d41f0bd2d3616a365b869bcf592d869343c2fdbed92a3491b" default))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(large-file-warning-threshold nil)
 '(org-agenda-files
   '("~/org/bioinformatics.org" "~/Dropbox/notes/reading/reading-log.org"))
 '(package-selected-packages
   '(rust-mode vterm julia-repl julia-mode flycheck elpygen python-mode elpy haskell-mode auctex exec-path-from-shell web-mode use-package solarized-theme org-ref org-pdfview helm-bibtexkey))
 '(python-shell-completion-native-disabled-interpreters '("pypy" "ipython" "python3"))
 '(python-shell-interpreter "python3")
 '(scheme-program-name "mit-scheme"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:height 1.0))))
 '(fixed-pitch ((t (:height 1.0))))
 )

;; ---------------------------------------------------------

(load-theme 'solarized t)

;; ---------------------------------------------------------

;; python-mode
;; (setq py-install-directory "~/.emacs.d/python-mode")
;; (add-to-list 'load-path py-install-directory)
;; (require 'python-mode)
;; (add-hook 'python-mode-hook (lambda ()
;; 			      ;; This breaks the blog export, as the
;; 			      ;; python snippet doesn't actually have
;; 			      ;; a filename. Need to investigate
;; 			      ;; flycheck for options. We'll just
;; 			      ;; spawn a new emacs without this
;; 			      ;; enabled for now.
;; 			      (setq fill-column 80)
;; 			      (flycheck-mode 1)
;; 			      (fci-mode 1)))
;; (add-to-list 'auto-mode-alist '("\\.py" . python-mode))

;; ---------------------------------------------------------

;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward)  ;; buffernames that are foo<1>, foo<2> are hard to read. This makes them foo|dir  foo|otherdir

;; ---------------------------------------------------------

; auto-complete
; (add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
; (require 'auto-complete)
; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
; (require 'auto-complete-config)
; (ac-config-default)
; (global-auto-complete-mode t)

;; ---------------------------------------------------------

;; From emacs-wiki
;; http://www.emacswiki.org/emacs/ToggleWindowSplit

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame
;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead
(global-set-key (kbd "C-x 5") 'toggle-frame-split)


;; ---------------------------------------------------------
;; markdown

(require 'ebib)

;; ---------------------------------------------------------
;; org-mode & LaTeX

; There should be a subdirectory of the home directory,
; possibly a link, which contains all the org files.
(setq jp-org-directory "~/org/")
(setq jp-bibliography-file (concat jp-org-directory "zotero.bib"))
(setq jp-reading-log-file (concat jp-org-directory "reading.org"))

(require 'org)
(require 'ox-latex)
(setq org-latex-create-formula-image-program 'dvipng)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((latex . t)))

;; displays an indented list of headings in agenda view
(setq org-tags-match-list-sublevels 'indented)
;; or display all subheadings without indentation:
;; (setq org-tags-match-list-sublevels t)
;; or don't display subheadings:
;; (setq org-tags-match-list-sublevels nil)

(global-set-key (kbd "C-c b") 'helm-bibtex)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("m" "Work Meetings")
        ("ma" "Pediatric GI Clinician Meeting"
         entry (file ,(concat jp-org-directory "work.org"))
         "* %u Pediatric GI Clinician Meeting\n%?"
         :clock-in t
         :clock-resume t)
        ("mb" "SMCS Pediatric Leadership Council"
         entry (file ,(concat jp-org-directory "work.org"))
         "* %u SMCS Pediatric Leadership Council :director\n%?"
         :clock-in t
         :clock-resume t)
        ("mc" "SMG Membership Meeting"
         entry (file ,(concat jp-org-directory "work.org"))
         "* %u SMG Membership Meeting\n%?"
         :clock-in t
         :clock-resume t)
        ("md" "Medical Director Meeting"
         entry (file ,(concat jp-org-directory "work.org"))
         "* %u Meeting :director\n%?"
         :clock-in t
         :clock-resume t)
        ("mo" "Other Work Meeting"
         entry (file ,(concat jp-org-directory "work.org"))
         "* %u Meeting\n%?"
         :clock-in t
         :clock-resume t)
        ("n" "note"
         entry (file ,(concat jp-org-directory "notes.org"))
         "* %? :NOTE:\n%U\n"
         :clock-in nil
         :clock-resume nil)
        ("?" "things to look up"
         entry (file ,(concat jp-org-directory "notes.org"))
         "* TTLU %? :%^g\n%U\n"
         :clock-in nil
         :clock-resume nil)
        ("N" "work-related note"
         entry (file ,(concat jp-org-directory "work.org"))
         "* %? :NOTE:\n%U\n"
         :clock-in nil
         :clock-resume nil)
       ))


;; ---------------------------------------------------------
;; org-ref

(require 'org-ref)
;; (require 'org-pdfview)
(require 'org-ref-pdf)
(require 'doi-utils)
(require 'org-ref-url-utils)
(require 'org-ref-bibtex)
(require 'org-ref-latex)
(require 'org-ref-arxiv)
(require 'org-ref-pubmed)
(require 'org-ref-isbn)
(require 'org-ref-wos)
(require 'org-ref-scopus)
(require 'org-ref-scifinder)
(require 'org-ref-worldcat)
(require 'org-ref-sci-id)
(require 'ffap)
(require 'helm-bibtex)

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))
;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)

(setq org-ref-helm-bibtex-action-preference 'mixed)
(setq org-ref-completion-library 'org-ref-helm-bibtex)

(setq reftex-default-bibliography jp-bibliography-file)
(setq org-ref-default-bibliography jp-bibliography-file
      org-ref-bibliography-notes jp-reading-log-file
      org-ref-pdf-directory (concat jp-org-directory "pdfs/"))

;; Use for one big notes file for all bibtex notes:
(setq bibtex-completion-notes-path jp-reading-log-file)
;; or use for one file per bibtex entry:
;; (setq bibtex-completion-notes-path "/path/to/notes/directory/")

(setq bibtex-completion-bibliography
      jp-bibliography-file)
(setq bibtex-completion-library-path
      (concat jp-org-directory "pdfs/"))

(setq bibtex-completion-notes-template-one-file "\
* ${author-or-editor} (${year}): ${title}
  :PROPERTIES:
  :Custom_ID: ${=key=}
  :Category:
  :Keywords: ${keywords}
  :DOI: ${doi}
  :URL: ${url}
  :END:
")

;; temporary to override pubmed-pmid-to-bibtex from org-ref
(load "~/jp-pubmed-to-bibtex.el")

;; ---------------------------------------------------------
;; programming
;; ---------------------------------------------------------

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :config
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   (setq-default flycheck-flake8-maximum-line-length 99)
;; )

(use-package vterm
  :ensure t)

;; ---------------------------------------------------------
;; python
;; ---------------------------------------------------------

;; https://realpython.com/emacs-the-best-python-editor/
;; https://jonathanabennett.github.io/blog/2019/06/20/python-and-emacs-pt.-1/

;; (require 'elpy)
;; (elpy-enable)
;;(use-package pyenv-mode
;;  :init
;;  (add-to-list 'exec-path "~/.pyenv/shims")
;;  (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;  :config
;;  (pyenv-mode)
;;  :bind
;;  ("C-x p e" . pyenv-activate-current-project))

(use-package elpy
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")
  (setenv "WORKON_HOME" "/Users/johann/opt/anaconda3/envs")
  :custom (elpy-rpc-backend "jedi")
)

;; (when (load "flycheck" t t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package python
  :ensure nil
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (elpy-enable))

;; ---------------------------------------------------------
;; julia
;; ---------------------------------------------------------

;; https://hershsingh.net/blog/emacs-julia/
;; https://github.com/tpapp/julia-repl

(defun my/julia-repl-send-cell()
  ;; "Send the current julia cell (delimited by ###) to the julia shell"
  (interactive)
  (save-excursion (setq cell-begin (if (re-search-backward "^###" nil t) (point) (point-min))))
  (save-excursion (setq cell-end (if (re-search-forward "^###" nil t) (point) (point-max))))
  (set-mark cell-begin)
  (goto-char cell-end)
  (julia-repl-send-region-or-line)
  (next-line))

(use-package julia-mode
  :ensure t)

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)

  :init
  (setenv "JULIA_NUM_THREADS" "8")

  :config
  ;; Set the terminal backend
  (julia-repl-set-terminal-backend 'vterm)

  ;; Keybindings for quickly sending code to the REPL
  (define-key julia-repl-mode-map (kbd "<C-RET>") 'my/julia-repl-send-cell)
  (define-key julia-repl-mode-map (kbd "<M-RET>") 'julia-repl-send-line)
  (define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer))

;; ---------------------------------------------------------
;; rust
;; ---------------------------------------------------------

(require 'rust-mode)

;;; .emacs ends here

;; ---------------------------------------------------------
;; assembly
;; ---------------------------------------------------------

(require 'gas-mode)

;; Enable asm-mode for ARM assembly files
(add-to-list 'auto-mode-alist '("\\.s\\'" . asm-mode))

;; Custom keywords for ARM assembly
(font-lock-add-keywords 'asm-mode
  '(("\\<\\(mov\\|ldr\\|bl\\|b\\|str\\|add\\|sub\\|mul\\|fmov\\|stp\\|ldp\\|svc\\|bl\\|ret\\|cmp\\|b\\(?:eq\\|ne\\|lt\\|le\\|gt\\|ge\\|mi\\|pl\\)\\)\\>"
     . font-lock-keyword-face)))

;; Custom indentation
(setq asm-comment-char ?@)
(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

;; ---------------------------------------------------------
;; Haskell
;; ---------------------------------------------------------

;; https://wiki.haskell.org/Emacs/Inferior_Haskell_processes

(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(put 'downcase-region 'disabled nil)
