;;; General emacs configuration stuff
(require 'package)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(setq inhibit-splash-screen t)
;; only type `y` instead of `yes`
(fset 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1) 
(tool-bar-mode -1) ;; no toolbar
(if window-system
    (progn
      (scroll-bar-mode -1) 
      (set-frame-font "Anonymous Pro-12"))) 
(put 'scroll-left 'disabled nil)
(load-theme 'zenburn t)

;; fuzzy matching on find-file, buffer switch
(ido-mode t)
(setq ido-enable-flex-matching t)
(add-to-list 'ido-ignore-files "\\.pyc")

;; (desktop-save-mode 1) ;; auto-save buffer state on close for a later time.
;; (setq abbrev-file-name "~/.emacs.d/abbrev_defs") ;; where to save auto-replace maps

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
;;
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
   (quote
    ("52fec4eebee0219de9b619c67ff72721fa8e462663b31e6573632e3e590ca900" "382956945efdbe0e39e06d6d7253f3bf05bdd98d2d82f1105dbe33b261338a46" "e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" "7e2b3b55e988a1828a11b706745c2d9b898879a0fb4fd9efdc5d86558c930b00" "6743c7238e9bd245782a680f92bee43261faf4997b66835c620fc64202b22663" "521e7ae4cbbbccc9f996d2292732b266afce23ef0de0e91236cc9629eb5f5a26" "76bb165fc9f375ec9f2308dabf1697e982f92ffd660a3cd933832da647df684d" "1838722404e4fe7d41f0bd2d3616a365b869bcf592d869343c2fdbed92a3491b" default)))
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 '(scheme-program-name "mit-scheme"))

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

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(put 'downcase-region 'disabled nil)
