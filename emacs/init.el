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
  ("M-Y" . yank-from-kill-ring)
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
(use-package evil
  :demand
  :init
  ;; these variables need to be set before loading evil
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-respect-visual-line-mode t)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
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
(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package treesit
  :config
  ;; js
  (add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.Dockerfile\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '(".babelrc\\'" . json-ts-mode))
  ;; bash/shell
  (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
  (add-to-list 'interpreter-mode-alist '("sh" . bash-ts-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js-ts-mode))
  ;; python
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode)))

(use-package apheleia
  :demand
  :bind
  (:map leader-map
	("c f" . apheleia-format-buffer))
  :config
  ;; more formatters
  (setf (alist-get 'prettier-json-stringify apheleia-formatters) ;; https://github.com/radian-software/apheleia/pull/183
	'(npx "prettier" "--stdin-filepath" filepath "--parser=json-stringify"))

  (setf (alist-get 'dockfmt apheleia-formatters) '("dockfmt" "fmt"))

  (setf (alist-get 'protobuf apheleia-formatters) '(npx "buf" "format" "--path" filepath)) ;; for .proto files https://github.com/bufbuild/buf

  ;; support for more languages
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent))
  (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier-markdown))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier-markdown))
  (add-to-list 'apheleia-mode-alist '(sh-mode . shfmt))
  (add-to-list 'apheleia-mode-alist '(bash-ts-mode . shfmt))
  (add-to-list 'apheleia-mode-alist '(protobuf-mode . protobuf))
  ;; (add-to-list 'apheleia-mode-alist '(dockerfile-ts-mode . dockfmt))

  (defun my/setup-fix-for-apheleia-prettier-package-json-files ()
    "Change formatter of package.json files to align with prettier default output."
    ;; https://github.com/radian-software/apheleia/pull/183
    (defun my/set-package-json-apheleia-formatter ()
      "Set the apheleia formatter to json-stringnify for package.json file."
      (when (equal (file-name-nondirectory (buffer-file-name)) "package.json")
	(setq-local apheleia-formatter 'prettier-json-stringify)))
    (add-hook 'json-ts-mode-hook 'my/set-package-json-apheleia-formatter))

  (my/setup-fix-for-apheleia-prettier-package-json-files)
  (apheleia-global-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package lsp-mode
  :demand
  :after (yasnippet) ;; https://github.com/emacs-lsp/lsp-mode/discussions/4033
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none)
  :hook ((;; js modes
	  js-ts-mode tsx-ts-mode typescript-ts-mode
	  ;; config files modes
	  json-ts-mode yaml-ts-mode
	  ;; css modes
	  scss-mode css-ts-mode
	  ;; others
	  lua-mode python-ts-mode) . lsp-deferred)
  :config
  (keymap-set leader-map "l" lsp-command-map))

(use-package consult-lsp
  :after (lsp-mode consult)
  :bind
  (:map lsp-command-map
	([remap xref-find-apropos] . consult-lsp-symbols)
	("f" . consult-lsp-diagnostics)))

(use-package which-key
  :config
  (which-key-mode))

;; treemacs
(use-package treemacs
  :custom
  (treemacs-pulse-on-success nil)
  (treemacs-width-is-initially-locked nil)
  :bind
  (:map project-prefix-map
	("t" . treemacs-display-current-project-exclusively))
  :config
  (treemacs-follow-mode))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

;; languages
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

(use-package nix-mode)

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package conf-mode
  :mode (("\\.npmrc\\'" . conf-mode)))

(use-package git-modes
  :mode ("\\.*ignore\\'" . gitignore-mode))

(use-package copilot
  :demand
  :hook (prog-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
	("M-C" . copilot-next-completion)
	("<tab>" . copilot-accept-completion)
	("TAB" . copilot-accept-completion)))
