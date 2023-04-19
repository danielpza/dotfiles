;; -*- lexical-binding: t -*-
(setq leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

;; my functions
(defun my/find-init-file ()
  "Open user-init-file."
  (interactive)
  (find-file "~/.config/home-manager/emacs/init.el"))

(defun my/find-home-manager-config ()
  "Open home manager config file."
  (interactive)
  (find-file "~/.config/home-manager/home.nix"))

(defun my/text-scale-increase ()
  "Increase font size."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun my/text-scale-decrease ()
  "Decrease font size."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

;; https://reddit.com/r/emacs/comments/2jzkz7/quickly_switch_to_previous_buffer/
(defun my/switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)))

;; core
(use-package emacs
  :bind
  ("<f6>" . load-theme)
  ("C--" . my/text-scale-decrease)
  ("C-=" . my/text-scale-increase)
  ("C-SPC" . completion-at-point)
  (:map leader-map
	("f f" . find-file)
	("f s" . save-buffer)
	("f r" . recentf)

	("d d" . my/find-init-file)
	("d n" . my/find-home-manager-config)

	("b b" . switch-to-buffer)
	("b d" . kill-current-buffer)
	("b r" . revert-buffer-quick)

	("TAB" . my/switch-to-last-buffer)
	("SPC" . execute-extended-command))
  :custom
  (revert-buffer-quick-short-answers t)
  (auto-save-default nil)
  (create-lockfiles nil) ;; react issues
  (make-backup-files nil) ;; react issues
  (tab-always-indent 'complete) ;; for corfu completions
  :config
  (recentf-mode)
  (global-display-line-numbers-mode)
  (global-goto-address-mode)
  (electric-pair-mode)
  (keymap-set leader-map "p" project-prefix-map))

;; completion
(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :custom
  (corfu-auto t) ;; Enable auto completion
  :config
  (global-corfu-mode))

;; git
(use-package magit
  :bind
  (:map leader-map
	("g g" . magit-status)))

;; evil
(use-package evil
  :init
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  :config
  (defun my/setup-evil-leader-key ()
    ;; leader key https://github.com/noctuid/evil-guide#preventing-certain-keys-from-being-overridden
    (define-minor-mode leader-mode
      "Bind leader-map to SPC prefix in evil modes"
      :lighter " Leader"
      :init-value t
      :global t
      :keymap (make-sparse-keymap))

    (dolist (state '(normal visual insert))
      (evil-make-intercept-map
       (evil-get-auxiliary-keymap leader-mode-map state t t)
       state))
    (evil-define-key '(normal visual) leader-mode-map (kbd "SPC") leader-map))

  ;; https://emacs.stackexchange.com/questions/14551/whats-the-difference-between-after-init-hook-and-emacs-startup-hook
  ;; (add-hook 'after-init-hook 'my/setup-evil-leader-key)
  (add-hook 'emacs-startup-hook 'my/setup-evil-leader-key)

  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-indent-plus
  :after evil
  :bind
  (:map evil-inner-text-objects-map
	("i" . evil-indent-plus-i-indent)
	("I" . evil-indent-plus-i-indent-up)
	("k" . evil-indent-plus-i-indent-up)
	("j" . evil-indent-plus-i-indent-up-down)
	("J" . evil-indent-plus-i-indent-up-down))
  (:map evil-outer-text-objects-map
	("i" . evil-indent-plus-a-indent)
	("I" . evil-indent-plus-a-indent-up)
	("J" . evil-indent-plus-a-indent-up-down)))

;; lang helpers
(use-package treesit
  :config
  ;; js
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '(".*rc\\'" . json-ts-mode))
  ;; python
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))

(use-package apheleia
  :config
  (apheleia-global-mode)
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier-markdown))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier-markdown)))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  ((nix-mode js-ts-mode json-ts-mode) . eglot-ensure))

;; languages
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package nix-mode)
