;; -*- lexical-binding: t -*-
(setq comp-deferred-compilation t)
(setq comp-async-report-warnings-errors nil) ;; silence compilation warnings

(custom-set-variables
 '(inhibit-startup-screen t) ;; remove startup screen
 '(ring-bell-function 'ignore) ;; disable bell

 ;; performance
 '(gc-cons-threshold 100000000) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
 '(read-process-output-max (* 1024 1024))) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process

;; disable ui elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; visual
(use-package doom-themes
  :disabled
  :config
  (load-theme 'doom-dark+ t))

(use-package ef-themes
  :config
  (load-theme 'ef-night t))

(use-package doom-modeline
  :config
  (doom-modeline-mode))
