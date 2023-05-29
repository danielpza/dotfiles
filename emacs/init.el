;; -*- lexical-binding: t -*-
(setq leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

(setup emacs
  (defun my/find-init-file ()
    "Open user-init-file."
    (interactive)
    (find-file "~/.config/home-manager/emacs/init.el"))

  (defun my/find-home-manager-config ()
    "Open home manager config file."
    (interactive)
    (find-file "~/.config/home-manager/common.nix"))

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
    (switch-to-buffer nil)))

;; core
(setup hideshow
  (:with-mode hs-minor-mode
    (:hook-into prog-mode)))

(setup global-keybindings
  (:global "<f6>" load-theme
	   "C--" my/text-scale-decrease
	   "C-=" my/text-scale-increase
	   "C-SPC" completion-at-point
	   "M-Y" yank-from-kill-ring))

(use-package emacs
  ;; :mode (("\\.npmrc\\'" . conf-mode))
  :bind
  ;; ("<f6>" . load-theme)
  ;; ("C--" . my/text-scale-decrease)
  ;; ("C-=" . my/text-scale-increase)
  ;; ("C-SPC" . completion-at-point)
  ;; ("M-Y" . yank-from-kill-ring)
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
  (fset 'yes-or-no-p 'y-or-n-p)
  (recentf-mode)
  (global-display-line-numbers-mode)
  (global-goto-address-mode)
  ;; (global-visual-line-mode)
  (electric-pair-mode)
  (keymap-set leader-map "p" project-prefix-map))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; diagnostic
(use-package flymake
  :disabled
  :demand
  :bind
  (:map leader-map
	("e n" . flymake-goto-next-error)
	("e p" . flymake-goto-prev-error)))

(use-package flycheck
  :demand
  :bind
  (:map leader-map
	("e l" . flycheck-list-errors)
	("e n" . flycheck-next-error)
	("e p" . flycheck-previous-error)))

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

(use-package kind-icon
  :disabled
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult
  :demand
  :bind
  ([remap project-find-regexp] . consult-ripgrep)
  ([remap isearch-forward] . consult-line)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap load-theme]. consult-theme)
  ([remap imenu] . consult-imenu)
  ([remap recentf] . consult-recent-file)
  (:map leader-map
	("e c" . consult-flycheck))
  :config
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;; git
(use-package magit
  :bind
  (:map leader-map
	("g b" . magit-blame)
	("g g" . magit-status)
	("g s" . magit-stage-file)
	("g l b" . magit-log-buffer-file)))

(use-package git-link
  :bind
  (:map leader-map
	("g l l" . git-link))
  :custom
  (git-link-use-commit t))

(use-package diff-hl
  :demand
  :bind
  (:map leader-map
	("g [" . diff-hl-previous-hunk)
	("g ]" . diff-hl-next-hunk))
  :config
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; evil
(setup (:require evil)
  (:also-load evil-collection)
  (:option evil-want-C-u-scroll t
	   evil-want-Y-yank-to-eol t
	   evil-undo-system 'undo-redo)
  (:global
   [remap evil-goto-definition] xref-find-definitions)
  (:bind-into evil-normal-state-map
    "z l" hs-hide-level)
  ;; (:map evil-normal-state-map ("z l" . hs-hide-level))
  ;; these variables need to be set before loading evil
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-respect-visual-line-mode t)
  (evil-mode))

(use-package evil
  ;; :demand
  ;; :init
  ;; ;; these variables need to be set before loading evil
  ;; (setq evil-want-integration t) ;; required by evil-collection
  ;; (setq evil-want-keybinding nil) ;; required by evil-collection
  ;; (setq evil-respect-visual-line-mode t)
  ;; :custom
  ;; (evil-want-C-u-scroll t)
  ;; (evil-want-Y-yank-to-eol t)
  ;; (evil-undo-system 'undo-redo)
  :bind
  ([remap evil-goto-definition] . xref-find-definitions)
  (:map evil-normal-state-map ("z l" . hs-hide-level))
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
  (add-hook 'after-init-hook 'my/setup-evil-leader-key)
  (add-hook 'emacs-startup-hook 'my/setup-evil-leader-key)

  (keymap-set leader-map "w" evil-window-map)

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
(setup editorconfig
  (:when-loaded (editorconfig-mode)))

(setup treesit
  (setq auto-mode-alist
	(append '(("\\Dockerfile\\'" . dockerfile-ts-mode)
		  ("\\.Dockerfile\\'" . dockerfile-ts-mode)
		  ;; js
		  ("\\.js\\'" . js-ts-mode)
		  ("\\.cjs\\'" . js-ts-mode)
		  ("\\.mjs\\'" . js-ts-mode)
		  ("\\.ts\\'" . typescript-ts-mode)
		  ("\\.mts\\'" . typescript-ts-mode)
		  ("\\.cts\\'" . typescript-ts-mode)
		  ("\\.tsx\\'" . tsx-ts-mode)
		  ("\\.ya?ml\\'" . yaml-ts-mode)
		  ("\\.css\\'" . css-ts-mode)
		  ("\\.json\\'" . json-ts-mode)
		  (".babelrc\\'" . json-ts-mode)
		  ;; bash/shell
		  ("\\.sh\\'" . bash-ts-mode)
		  ;; python
		  ("\\.py\\'" . python-ts-mode))
		auto-mode-alist))
  (setq interpreter-mode-alist
	(append '(("node" . js-ts-mode)
		  ("sh" . bash-ts-mode))
		interpreter-mode-alist)))

(setup apheleia
  (:bind-into leader-map
    "c f" apheleia-format-buffer)
  ;; more formatters
  (:when-loaded
    (setq apheleia-formatters
	  (append '((prettier-json-stringify . (npx "prettier" "--stdin-filepath" filepath "--parser=json-stringify")) ;; https://github.com/radian-software/apheleia/pull/183
		    (dockfmt . ("dockfmt" "fmt"))
		    (protobuf . (npx "buf" "format" "--path" filepath)) ;; for .proto files https://github.com/bufbuild/buf
		    )
		  apheleia-formatters))

    ;; (setf (alist-get 'protobuf apheleia-formatters) ') 
    ;; support for more languages
    (setq apheleia-mode-alist
	  (append '((emacs-lisp-mode . lisp-indent)
		    (gfm-mode . prettier-markdown)
		    (markdown-mode . prettier-markdown)
		    (sh-mode . shfmt)
		    (bash-ts-mode . shfmt)
		    (protobuf-mode . protobuf)
		    ;; (dockerfile-ts-mode . dockfmt)
		    )
		  apheleia-mode-alist)))
  (apheleia-global-mode))

(setup lsp-mode
  (yas-global-mode 1) ;; required for json-mode to load https://github.com/emacs-lsp/lsp-mode/discussions/4033
  (:also-load consult-lsp)
  (:option lsp-completion-provider :none)
  (:with-function lsp-deferred
    (:hook-into
     ;; js modes
     js-ts-mode tsx-ts-mode typescript-ts-mode
     ;; config files modes
     json-ts-mode yaml-ts-mode
     ;; css modes
     scss-mode css-ts-mode
     ;; others
     lua-mode python-ts-mode))
  (setup consult-lsp
    (:when-loaded (keymap-set leader-map "l" lsp-command-map))
    (:bind-into lsp-command-map
      [remap xref-find-apropos] consult-lsp-symbols
      "f"  consult-lsp-diagnostics)))

(setup which-key
  (which-key-mode))

;; treemacs
(setup treemacs
  (:option treemacs-pulse-on-success nil
	   treemacs-width-is-initially-locked nil)
  (:bind-into project-prefix-map
    "t" treemacs-display-current-project-exclusively)
  ;; (:when-loaded (treemacs-follow-mode))
  )

;; languages
(setup markdown-mode
  (:with-mode gfm-mode
    (:file-match "README\\.md\\'"))
  (:bind-into markdown-mode-map
    "C-c C-e" markdown-do))

(setup protobuf-mode
  (:file-match "\\.proto\\'"))

(setup git-modes
  (:with-mode gitignore-mode
    (:file-match "\\.*ignore\\'")))

(setup (:require copilot)
  (:hook-into prog-mode)
  (:bind-into copilot-completion-map
    "M-C" copilot-next-completion
    "<tab>" copilot-accept-completion
    "TAB" copilot-accept-completion))

(setup conf-mode
  (:file-match "\\.npmrc\\'"))
