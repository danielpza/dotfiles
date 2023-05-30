;; -*- lexical-binding: t -*-
(setq leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

;; core
(custom-set-variables
 '(auto-save-default nil)
 '(ispell-program-name "aspell") ;; use aspell instead of ispell
 '(use-short-answers t) ;; use y/n instead of yes/no
 '(revert-buffer-quick-short-answers t) ;; use y/n instead of yes/no
 '(create-lockfiles nil) ;; react issues
 '(make-backup-files nil) ;; react issues
 '(dired-mouse-drag-files t)
 '(mouse-drag-and-drop-region-cross-program t)
 '(dired-listing-switches "--almost-all --human-readable --group-directories-first -l --no-group")
 '(tab-always-indent 'complete)) ;; for corfu completions

(recentf-mode)
(global-display-line-numbers-mode)
(global-goto-address-mode) ;; make URLs clickable
;; (global-visual-line-mode)
(electric-pair-mode) ;; auto close brackets
(add-hook 'prog-mode-hook 'hs-minor-mode) ;; code folding
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ;; spell check in comments

;; core improvements
(use-package nerd-icons)

(use-package nerd-icons-completion
  :after nerd-icons
  :config
  (nerd-icons-completion-mode))

(use-package dirvish
  :config
  (custom-set-variables '(dirvish-subtree-state-style 'nerd)
			'(dirvish-attributes '(vc-state
					       subtree-state nerd-icons collapse ;; git-msg
					       ;; file-time file-size
					       )))
  (bind-keys :map project-prefix-map
	     ("t" . dirvish-side))
  (bind-keys :map dired-mode-map
	     ("<mouse-1>" . dirvish-subtree-toggle-or-open)
             ([remap dired-sort-toggle-or-edit] . dirvish-quicksort)
             ([remap dired-do-redisplay] . dirvish-ls-switches-menu)
             ([remap dired-do-copy] . dirvish-yank-menu)
	     ("?" . dirvish-dispatch)
             ("q" . dirvish-quit)
             ("a" . dirvish-quick-access)
             ("f" . dirvish-file-info-menu)
             ("x" . dired-do-delete)
             ("X" . dired-do-flagged-delete)
             ("y" . dirvish-yank-menu)
             ("s" . dirvish-quicksort)
	     ;; ("<return>" . dirvish-subtree-toggle)
             ("<tab>" . dirvish-subtree-toggle)
	     ("TAB" . dirvish-subtree-toggle))
  (dirvish-override-dired-mode))

;; diagnostic
(use-package flycheck
  :config
  (bind-keys :map leader-map
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
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-initialism orderless-prefixes orderless-regexp))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :custom
  (corfu-auto t) ;; Enable auto completion
  :config
  (global-corfu-mode))

(use-package consult
  :config
  (bind-keys ([remap project-find-regexp] . consult-ripgrep)
	     ([remap isearch-forward] . consult-line)
	     ([remap switch-to-buffer] . consult-buffer)
	     ([remap load-theme]. consult-theme)
	     ([remap imenu] . consult-imenu)
	     ([remap recentf] . consult-recent-file))
  (bind-keys :map leader-map
	     ("e c" . consult-flycheck))
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

;; git
(use-package magit
  :config
  (bind-keys :map leader-map
	     ("g b" . magit-blame)
	     ("g g" . magit-status)
	     ("g s" . magit-stage-file)
	     ("g l b" . magit-log-buffer-file)))

(use-package git-link
  :config
  (bind-keys :map leader-map
	     ("g l l" . git-link))
  :custom
  (git-link-use-commit t))

(use-package diff-hl
  :config
  (bind-keys :map leader-map
	     ("g [" . diff-hl-previous-hunk)
	     ("g ]" . diff-hl-next-hunk))
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; evil
(use-package evil
  :init
  ;; these variables need to be set before loading evil
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  (setq evil-respect-visual-line-mode t)
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  :config
  (bind-keys ([remap evil-goto-definition] . xref-find-definitions))
  (bind-keys :map evil-normal-state-map
	     ("z l" . hs-hide-level))
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
  (setq auto-mode-alist (append '(("\\.?Dockerfile\\'" . dockerfile-ts-mode)
				  ;; js
				  ("\\.[cm]?jsx?\\'" . js-ts-mode)
				  ("\\.[cm]?tsx?\\'" . typescript-ts-mode)
				  ;; css
				  ("\\.css\\'" . css-ts-mode)
				  ;; json
				  ("\\.json\\'" . json-ts-mode)
				  (".babelrc\\'" . json-ts-mode)
				  ;; yaml
				  ("\\.ya?ml\\'" . yaml-ts-mode)
				  ;; bash/shell
				  ("\\.sh\\'" . bash-ts-mode)
				  ;; python
				  ("\\.py\\'" . python-ts-mode))
				auto-mode-alist))
  (setq interpreter-mode-alist
	(append '(("node" . js-ts-mode)
		  ("bash" . bash-ts-mode)
		  ("sh" . bash-ts-mode)
		  ("python" . python-ts-mode))
		interpreter-mode-alist)))

(use-package apheleia
  :config
  (bind-keys :map leader-map
	     ("c f" . apheleia-format-buffer))

  ;; more formatters
  (setq apheleia-formatters (append '(
				      (dockfmt . ("dockfmt" "fmt"))
				      (protobuf . ("buf" "format" "--path" filepath))  ;; for .proto files https://github.com/bufbuild/buf
				      (prettier-json-stringify . (npx "prettier" "--stdin-filepath" filepath "--parser=json-stringify")) ;; https://github.com/radian-software/apheleia/pull/183
				      ) apheleia-formatters))

  (setq apheleia-mode-alist
	(append '((emacs-lisp-mode . lisp-indent)
		  (gfm-mode . prettier-markdown)
		  (markdown-mode . prettier-markdown)
		  (sh-mode . shfmt)
		  (bash-ts-mode . shfmt)
		  (protobuf-mode . protobuf)
		  ;; (dockerfile-ts-mode . dockfmt)
		  ) apheleia-mode-alist))

  ;; support for more languages
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

;; languages
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

(use-package nix-mode)

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package git-modes
  :mode ("\\.*ignore\\'" . gitignore-mode))

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :config
  (bind-keys :map copilot-completion-map
	     ("M-C" . copilot-next-completion)
	     ("<tab>" . copilot-accept-completion)
	     ("TAB" . copilot-accept-completion)))

(add-to-list 'auto-mode-alist '("\\.npmrc\\'" . conf-mode))

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

;; general keybindings
(bind-keys ("<f6>" . load-theme)
	   ("C--" . my/text-scale-decrease)
	   ("C-=" . my/text-scale-increase)
	   ("C-SPC" . completion-at-point)
	   ("M-Y" . yank-from-kill-ring))

;; leader keybindings
(bind-keys :map leader-map
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

(keymap-set leader-map "p" project-prefix-map)
(keymap-set leader-map "h" help-map)
