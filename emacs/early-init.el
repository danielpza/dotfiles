;; -*- lexical-binding: t -*-
(setq comp-deferred-compilation t)
(setq comp-async-report-warnings-errors nil) ;; silence compilation warnings
(setq inhibit-startup-screen t) ;; remove startup screen

;; performance
(setq gc-cons-threshold 100000000) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(setq read-process-output-max (* 1024 1024)) ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process

;; disable ui elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(customize-set-variable 'ring-bell-function 'ignore)

;; visual
(require 'doom-themes)
(load-theme 'doom-dark+ t)

(require 'doom-modeline)
(doom-modeline-mode)
