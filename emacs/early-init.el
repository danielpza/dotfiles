;; -*- lexical-binding: t -*-
(setq comp-deferred-compilation t)
(setq comp-async-report-warnings-errors nil) ;; silence compilation warnings
(setq inhibit-startup-screen t) ;; remove startup screen

(let ((prev-gc-threshold gc-cons-threshold))
  ;; increase gc threshold while loading to make loading faster
  (setq gc-cons-threshold (* 50 1000 1000))
  (add-hook 'emacs-startup-hook (lambda ()
				  ;; (setq gc-cons-threshold (* 2 1000 1000))
				  (setq gc-cons-threshold prev-gc-threshold)
				  (setq read-process-output-max (* 1024 1024)) ;; 1mb
				  )))

;; disable ui elements
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(customize-set-variable 'ring-bell-function 'ignore)

;; visual
(use-package doom-themes
  :config
  (load-theme 'doom-dark+ t))

(use-package doom-modeline
  :config
  (doom-modeline-mode))
