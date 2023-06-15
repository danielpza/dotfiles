;; -*- lexical-binding: t -*-
(defvar leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

;; core
(setopt
 auto-save-default nil
 ispell-program-name "hunspell" ;; use hunspell instead of ispell
 use-short-answers t ;; use y/n instead of yes/no
 revert-buffer-quick-short-answers t ;; use y/n instead of yes/no
 create-lockfiles nil ;; lockfiles causes issues with react-scripts
 make-backup-files nil ;; backup files causes issues with react-scripts
 tab-always-indent 'complete ;; for corfu completions
 ;; dired related
 dired-mouse-drag-files t ;; allow drag and drop from dired
 mouse-drag-and-drop-region-cross-program t ;; allow drag and drop from emacs to other programs
 dired-listing-switches "--almost-all --human-readable --group-directories-first -l --no-group")

(recentf-mode)
(global-display-line-numbers-mode)
(global-goto-address-mode) ;; make URLs clickable
;; (global-visual-line-mode)
(electric-pair-mode) ;; auto close brackets
(add-hook 'prog-mode-hook 'hs-minor-mode) ;; code folding

(require 'hideshow)

;; evil
(use-package evil
  :defines evil-normal-state-map evil-window-map
  :functions evil-define-key evil-make-intercept-map evil-get-auxiliary-keymap evil-mode
  :init
  ;; these variables need to be set before loading evil
  (setopt evil-want-integration t ;; required by evil-collection
	  evil-want-keybinding nil ;; required by evil-collection
	  evil-respect-visual-line-mode t)
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
  :functions evil-collection-init
  :after evil
  :config
  (evil-collection-init))

(use-package evil-indent-plus
  :defines evil-inner-text-objects-map evil-outer-text-objects-map
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

;; core improvements
(use-package nerd-icons)

(use-package nerd-icons-completion
  :functions nerd-icons-completion-mode
  :after nerd-icons
  :config
  (nerd-icons-completion-mode))

(require 'dired)

(use-package dirvish
  :demand
  :commands (dired project-switch-project)
  :defines dirvish-subtree-state-style dirvish-attributes dirvish-mode-map
  :functions dirvish-dwim dirvish-side dirvish-subtree-up dirvish-subtree-toggle-or-open dirvish-quicksort dirvish-ls-switches-menu dirvish-yank-menu dirvish-dispatch dirvish-quit dirvish-quick-access dirvish-file-info-menu dirvish-subtree-toggle dirvish-override-dired-mode
  :bind
  ("C-x d" . dirvish-dwim)
  (:map leader-map
	("a d" . dirvish-dwim))
  (:map project-prefix-map
	("t" . dirvish-side))
  :config
  (custom-set-variables '(dirvish-subtree-state-style 'nerd)
			'(dirvish-attributes '(;; vc-state
					       subtree-state nerd-icons collapse
					       ;; git-msg file-time file-size
					       )))
  (bind-keys :map dired-mode-map
	     ("u" . dirvish-subtree-up))
  (bind-keys :map dirvish-mode-map
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
	     ("u" . dirvish-subtree-up)
	     ;; ("<return>" . dirvish-subtree-toggle)
	     ("<tab>" . dirvish-subtree-toggle)
	     ("TAB" . dirvish-subtree-toggle))
  (dirvish-override-dired-mode))

;; diagnostic
(use-package jinx
  :functions global-jinx-mode
  ;; spell checking
  :config
  (global-jinx-mode))

(use-package flycheck
  :functions global-flycheck-mode flycheck-list-errors flycheck-next-error flycheck-previous-error
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; https://emacs.stackexchange.com/questions/21664/how-to-prevent-flycheck-from-treating-my-init-el-as-a-package-file
  :config
  (bind-keys :map leader-map
	     ("e l" . flycheck-list-errors)
	     ("e n" . flycheck-next-error)
	     ("e p" . flycheck-previous-error))
  (global-flycheck-mode))

;; completion
(use-package vertico
  :functions vertico-mode
  :config
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-initialism orderless-prefixes orderless-regexp))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :functions global-corfu-mode
  :custom
  (corfu-auto t) ;; Enable auto completion
  :config
  (global-corfu-mode))

(use-package consult
  :functions consult-ripgrep consult-line consult-buffer consult-theme consult-imenu consult-recent-file consult-flycheck consult-xref
  :config
  (bind-keys ([remap project-find-regexp] . consult-ripgrep)
	     ([remap isearch-forward] . consult-line)
	     ([remap switch-to-buffer] . consult-buffer)
	     ([remap load-theme]. consult-theme)
	     ([remap imenu] . consult-imenu)
	     ([remap recentf] . consult-recent-file))
  (bind-keys :map leader-map
	     ("e c" . consult-flycheck))
  (setopt xref-show-xrefs-function #'consult-xref
	  xref-show-definitions-function #'consult-xref))

;; git
(use-package magit
  :functions magit-blame magit-status magit-stage-file magit-log-buffer-file
  :config
  (bind-keys :map leader-map
	     ("g b" . magit-blame)
	     ("g g" . magit-status)
	     ("g s" . magit-stage-file)
	     ("g l b" . magit-log-buffer-file)))

(use-package git-link
  :functions git-link
  :config
  (bind-keys :map leader-map
	     ("g l l" . git-link))
  :custom
  (git-link-use-commit t))

(use-package diff-hl
  :functions diff-hl-previous-hunk diff-hl-next-hunk diff-hl-magit-pre-refresh diff-hl-magit-post-refresh diff-hl-flydiff-mode global-diff-hl-mode
  :config
  (bind-keys :map leader-map
	     ("g [" . diff-hl-previous-hunk)
	     ("g ]" . diff-hl-next-hunk))
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; (use-package topsy
;;   :hook prog-mode)

;; lang helpers
(use-package editorconfig
  :functions editorconfig-mode
  :config
  (editorconfig-mode))

(use-package treesit
  :mode
  ("\\.Dockerfile\\'" . dockerfile-ts-mode)
  ("Dockerfile\\'" . dockerfile-ts-mode)
  ;; js
  ("\\.[cm]?jsx?\\'" . js-ts-mode)
  ("\\.[cm]?ts\\'" . typescript-ts-mode)
  ("\\.[cm]?tsx\\'" . tsx-ts-mode)
  ;; css
  ("\\.css\\'" . css-ts-mode)
  ;; json
  ("\\.json\\'" . json-ts-mode)
  ("\\.*rc\\'" . json-ts-mode) ;; .babelrc .prettierrc and other .rc javascript configuration files
  ;; yaml
  ("\\.ya?ml\\'" . yaml-ts-mode)
  ;; bash/shell
  ("\\.sh\\'" . bash-ts-mode)
  ;; python
  ("\\.py\\'" . python-ts-mode)
  :interpreter
  ("node" . js-ts-mode)
  ("bash" . bash-ts-mode)
  ("sh" . bash-ts-mode)
  ("python" . python-ts-mode))

(use-package apheleia
  :defines apheleia-formatters apheleia-mode-alist
  :functions apheleia-format-buffer apheleia-global-mode
  :config
  (bind-keys :map leader-map
	     ("c f" . apheleia-format-buffer))

  ;; more formatters
  (setq apheleia-formatters (append '((dprint . ("dprint" "fmt" "--stdin" filepath "--config" "/home/daniel/.config/dprint/config.json"))
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
		  (dockerfile-ts-mode . dprint)
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

(use-package copilot
  :defines copilot-completion-map
  :functions copilot-mode copilot-next-completion copilot-accept-completion
  :hook ((prog-mode markdown-mode conf-mode yaml-ts-mode) . copilot-mode)
  :config
  (bind-keys :map copilot-completion-map
	     ("M-C" . copilot-next-completion)
	     ("<tab>" . copilot-accept-completion)
	     ("TAB" . copilot-accept-completion)))

(use-package yasnippet
  :functions yas-global-mode
  :config
  (yas-global-mode 1))

(use-package lsp-mode
  :after (yasnippet) ;; https://github.com/emacs-lsp/lsp-mode/discussions/4033
  :commands (lsp lsp-deferred)
  :functions lsp-inlay-hints-mode
  :defines lsp-command-map
  :custom
  (lsp-completion-provider :none)
  (lsp-inlay-hint-enable t)
  (lsp-javascript-display-enum-member-value-hints t)
  (lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
  (lsp-javascript-display-parameter-type-hints t)
  (lsp-javascript-display-property-declaration-type-hints t)
  (lsp-javascript-display-return-type-hints t)
  (lsp-javascript-display-variable-type-hints t)
  ;; (lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  :hook ((;; js modes
	  js-ts-mode tsx-ts-mode typescript-ts-mode
	  ;; config files modes
	  json-ts-mode yaml-ts-mode
	  ;; css modes
	  scss-mode css-ts-mode
	  ;; others
	  lua-mode python-ts-mode nix-mode) . lsp-deferred)
  :config
  (lsp-inlay-hints-mode)
  (keymap-set leader-map "l" lsp-command-map))

(use-package consult-lsp
  :after (lsp-mode consult)
  :bind
  (:map lsp-command-map
	([remap xref-find-apropos] . consult-lsp-symbols)
	("f" . consult-lsp-diagnostics)))

(use-package which-key
  :functions which-key-mode
  :config
  (which-key-mode))

;; languages
(use-package markdown-mode
  :defines markdown-mode-map
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map
	      ("C-c C-e" . markdown-do)))

(use-package nix-mode)

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package git-modes
  :mode ("\\.*ignore\\'" . gitignore-mode))

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

;; https://emacs.stackexchange.com/questions/76678/how-to-get-help-on-a-symbol-at-point-with-f1
(defun my/describe-symbol ()
  (interactive)
  (describe-symbol (symbol-at-point)))

(defmacro my/hs-hide-level (level)
  `(progn
     (defun ,(intern (format "my/hs-hide-level-%d" level)) ()
       (interactive)
       (hs-hide-level ,level))
     (bind-keys (,(format "M-%d" level) . ,(intern (format "my/hs-hide-level-%d" level))))))

(my/hs-hide-level 1)
(my/hs-hide-level 2)
(my/hs-hide-level 3)
(my/hs-hide-level 4)
(my/hs-hide-level 5)

;; general keybindings
(bind-keys ("<f1>" . my/describe-symbol)
	   ("<f6>" . load-theme)
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
