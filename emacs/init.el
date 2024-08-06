;; -*- lexical-binding: t -*-
(defvar leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

;; core
(setopt
 frame-resize-pixelwise t
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
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 dired-listing-switches "--almost-all --human-readable --group-directories-first -l --no-group"
 tab-width 4 ;; 8 as default is too much
 )

(recentf-mode)
(global-display-line-numbers-mode)
(global-goto-address-mode) ;; make URLs clickable
;; (global-visual-line-mode)
(electric-pair-mode) ;; auto close brackets
(add-hook 'prog-mode-hook 'hs-minor-mode) ;; code folding
(add-hook 'conf-unix-mode-hook 'hs-minor-mode)

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
  (evil-want-C-w-delete nil)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  (evil-shift-width 2)
  (evil-echo-state nil)
  :config
  (bind-keys :map evil-insert-state-map ("<backtab>" . evil-shift-left-line))
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
  :custom
  (evil-collection-key-blacklist '("SPC"))
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
  ;; :commands (dired project-switch-project)
  :defines dirvish-subtree-state-style dirvish-attributes dirvish-mode-map
  :functions dirvish-dwim dirvish-side dirvish-subtree-up dirvish-subtree-toggle-or-open dirvish-quicksort dirvish-ls-switches-menu dirvish-yank-menu dirvish-dispatch dirvish-quit dirvish-quick-access dirvish-file-info-menu dirvish-subtree-toggle dirvish-override-dired-mode
  :bind
  ("C-x d" . dirvish-dwim)
  (:map leader-map
		("a d" . dirvish-dwim))
  ;; (:map project-prefix-map
  ;; 		("t" . dirvish-side))
  :custom
  (dirvish-header-line-height 20)
  (dirvish-mode-line-height 20)
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

(use-package treemacs-nerd-icons
  ;; :after (treemacs nerd-icons)
  :functions treemacs-load-theme
  :config
  (treemacs-load-theme "nerd-icons"))

;; diagnostic
(use-package jinx
  :disabled
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

(use-package company
  :disabled
  :functions global-company-mode
  :bind
  ([remap completion-at-point] . company-capf)
  :config
  (global-company-mode))

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

(use-package cape
  :disabled
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-emoji)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package embark
  :bind
  (("M-e" . embark-act)         ;; pick some comfortable binding
   ;; ("M-E" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; git
(use-package magit
  :functions magit-blame magit-status magit-stage-file magit-log-buffer-file magit-refresh-all
  :config
  (bind-keys :map leader-map
			 ("g b" . magit-blame)
			 ("g g" . magit-status)
			 ("g s" . magit-stage-file)
			 ("g l b" . magit-log-buffer-file))
  ;; (add-to-list 'display-buffer-alist
  ;; 			   '("magit.*:"
  ;; 				 (display-buffer-reuse-window display-buffer-same-window)))
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1) ;; https://stackoverflow.com/questions/9439702/how-to-open-magit-status-in-full-window
  )

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
  ("\\.?Dockerfile\\'" . dockerfile-ts-mode)
  ;; js
  ("\\.[cm]?jsx?\\'" . js-ts-mode)
  ("\\.[cm]?ts\\'" . typescript-ts-mode)
  ("\\.[cm]?tsx\\'" . tsx-ts-mode)
  ;; css
  ("\\.css\\'" . css-ts-mode)
  ;; json
  ("\\.json\\'" . json-ts-mode)
  ("\\..*rc\\'" . json-ts-mode) ;; .babelrc .prettierrc, etc
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
  ;; :custom
  ;; (apheleia-log-debug-info t)
  :config
  (bind-keys :map leader-map
			 ("c f" . apheleia-format-buffer))

  ;; more formatters
  (setq apheleia-formatters (append '((dprint . ("dprint" "fmt" "--stdin" filepath "--config" "/home/daniel/.config/dprint/config.json"))
									  (protobuf . ("buf" "format" "--path" filepath))  ;; for .proto files https://github.com/bufbuild/buf
									  (prettier-json-stringify . ("apheleia-npx" "prettier" "--stdin-filepath" filepath "--parser=json-stringify")) ;; https://github.com/radian-software/apheleia/pull/183
									  (gdformat . ("gdformat" "-"))
									  ) apheleia-formatters))

  (setq apheleia-mode-alist
		(append '((emacs-lisp-mode . lisp-indent)
				  (gfm-mode . prettier-markdown)
				  (markdown-mode . prettier-markdown)
				  (sh-mode . shfmt)
				  (bash-ts-mode . shfmt)
				  (protobuf-mode . protobuf)
				  (dockerfile-ts-mode . dprint)
				  (gdscript-ts-mode . gdformat)
				  ;; (mhtml-mode . prettier-html)
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

  ;; https://github.com/radian-software/apheleia/issues/30#issuecomment-778150037
  (defun shou/fix-apheleia-project-dir (orig-fn &rest args)
    (let ((project (project-current)))
      (if (not (null project))
          (let ((default-directory (project-root project))) (apply orig-fn args))
        (apply orig-fn args))))
  (advice-add 'apheleia-format-buffer :around #'shou/fix-apheleia-project-dir)

  (my/setup-fix-for-apheleia-prettier-package-json-files)
  (apheleia-global-mode))

(use-package copilot
  :disabled

  :defines copilot-completion-map
  :functions copilot-mode copilot-next-completion copilot-accept-completion copilot-diagnose
  :hook ((prog-mode markdown-mode conf-mode yaml-ts-mode) . copilot-mode)
  :custom
  ;; https://github.com/copilot-emacs/copilot.el/pull/230, https://github.com/copilot-emacs/copilot.el/issues/220
  (copilot-indent-offset-warning-disable t)
  (copilot--indent-warning-printed-p t)
  :config
  (bind-keys ("M-S" . copilot-diagnose)
			 :map copilot-completion-map
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
  (lsp-inlay-hint-enable nil)
  (lsp-javascript-display-enum-member-value-hints t)
  (lsp-javascript-display-parameter-name-hints-when-argument-matches-name t)
  (lsp-javascript-display-parameter-type-hints t)
  (lsp-javascript-display-property-declaration-type-hints t)
  (lsp-javascript-display-return-type-hints nil)
  (lsp-javascript-display-variable-type-hints t)
  (lsp-auto-execute-action nil)
  (lsp-eslint-server-command '("vscode-eslint-language-server" "--stdio"))
  (lsp-eslint-auto-fix-on-save t)
  (lsp-auto-guess-root t)
  (lsp-enable-suggest-server-download nil) ;; will be managed by home-manager
  (lsp-references-exclude-definition t)
  :bind
  ;; ("M-i" . lsp-inlay-hints-mode)
  ("M-r" . lsp-rename)
  ("M-s" . lsp)
  ("M-W" . lsp-workspace-restart)
  ("M-a" . lsp-execute-code-action)
  ("M-R" . lsp-javascript-rename-file)
  :hook ((;; js modes
		  js-ts-mode tsx-ts-mode typescript-ts-mode
		  ;; config files modes
		  json-ts-mode yaml-ts-mode
		  ;; css modes
		  scss-mode css-ts-mode
		  ;; godot
		  gdscript-ts-mode
		  ;; others
		  bash-ts-mode
		  gfm-mode markdown-mode
		  dockerfile-ts-mode terraform-mode
		  lua-mode python-ts-mode nix-mode) . lsp-deferred)
  :config
  (lsp-inlay-hints-mode)
  (keymap-set leader-map "l" lsp-command-map))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t))

(use-package lsp-pyright
  :after (lsp-mode))

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
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.mdx\\'" . markdown-mode)
  :bind (:map markdown-mode-map
			  ("C-c C-e" . markdown-do)))

(use-package nix-mode)

(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package git-modes
  :mode ("\\.*ignore\\'" . gitignore-mode))

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'hs-minor-mode))

(use-package earthfile-mode)

(use-package feature-mode
  :mode ("\\.feature\\'" . feature-mode)
  :defines feature-default-language
  :config
  (setq feature-default-language "fi"))

(use-package org
  :bind (:map org-mode-map
			  ("M-S" . org-sort)
			  ))

(add-to-list 'auto-mode-alist '("\\.npmrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.env.*\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.godot\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.import\\'" . conf-mode))

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

;; https://emacs.stackexchange.com/questions/76678/how-to-get-help-on-a-symbol-at-point-with-f1
(defun my/describe-symbol ()
  (interactive)
  (describe-symbol (symbol-at-point)))

(defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  ;; https://zck.org/deleting-files-in-emacs
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
                 (file-exists-p filename))
        (delete-file filename))
      (kill-buffer buffer))))

(defun my/locate-dominating-file (FILE NAME)
  "Like locate-dominating-file but return the file path instead of the directory."
  (when-let ((dir (locate-dominating-file FILE NAME)))
	(file-name-concat dir NAME)))

(defun my/open-nearest-file (FILE)
  "Search for FILE in current directory. If not found searches in the parent."
  (interactive "sFile: ")
  (if-let ((file (my/locate-dominating-file default-directory FILE)))
	  (find-file file)
	(message "File %s not found" FILE)))

(defun my/open-nearest-manifest-file ()
  "Searches for package.json or pyproject.toml in current directory.
If not found searches in the parent."
  (interactive)
  (if-let ((file (or (my/locate-dominating-file default-directory "package.json")
					 (my/locate-dominating-file default-directory "pyproject.toml")
					 (my/locate-dominating-file default-directory "flake.nix"))))
	  (find-file file)
	(message "No package.json or pyproject.toml found")))

(defun my/open-nearest-flake-nix ()
  (interactive)
  (my/open-nearest-file "flake.nix"))

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
		   ("f R" . rename-visited-file)
		   ("f n" . my/open-nearest-flake-nix)
		   ("f p" . my/open-nearest-manifest-file)

		   ("d d" . my/find-init-file)
		   ("d n" . my/find-home-manager-config)

		   ("l s" . sort-lines)

		   ("b b" . switch-to-buffer)
		   ("b d" . kill-current-buffer)
		   ("b r" . revert-buffer-quick)

		   ("TAB" . my/switch-to-last-buffer)
		   ("SPC" . execute-extended-command))

(keymap-set leader-map "p" project-prefix-map)
(keymap-set leader-map "h" help-map)

;; load custom config file if exists
(let ((custom-file (concat user-emacs-directory "init.custom.el")))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; https://stackoverflow.com/questions/13981899/how-can-i-kill-all-buffers-in-my-emacs
(defun my/kill-all-buffers ()
  (interactive)
  ;; kill all file buffers except current buffer
  (dolist (buffer (buffer-list))
    (unless (or (string-match-p "^\\*" (buffer-name buffer))
				(eq buffer (current-buffer)))
      (kill-buffer buffer))))

;; bind spc b K to kill all buffers
(bind-keys :map leader-map
		   ("b K" . my/kill-all-buffers))

(defun my/insert-line-before (message)
  ;; https://stackoverflow.com/questions/622440/emacs-command-to-insert-and-indent-line-above-cursor
  ;; https://emacs.stackexchange.com/questions/32958/insert-line-above-below
  (save-excursion
    (beginning-of-line)
    (end-of-line 0)
    (newline-and-indent)
    (insert message)))

(defun my/insert-line-after (message)
  (save-excursion
    (end-of-line)
    (newline-and-indent)
    (insert message)))

(defun my/insert-line-after (message)
  (save-excursion
    (end-of-line)
    (newline-and-indent)
    (insert message)))

(defun my/insert-ts-expect-error ()
  (interactive)
  (my/insert-line-before "// @ts-expect-error -- TODO fix later"))

(defun my/insert-ts-expect-error-tsx ()
  (interactive)
  (my/insert-line-before "{/* @ts-expect-error -- TODO fix later */}"))

(defun my/insert-eslint-disable-next-line ()
  (interactive)
  (my/insert-line-before "// eslint-disable-next-line"))

(defun my/insert-eslint-disable-next-line-tsx ()
  (interactive)
  (my/insert-line-before "{/* eslint-disable-next-line */}"))

(bind-keys ("M-i" . my/insert-ts-expect-error)
		   ("M-I" . my/insert-ts-expect-error-tsx)
		   ;; ("M-e" . my/insert-eslint-disable-next-line)
		   ;; ("M-E" . my/insert-eslint-disable-next-line-tsx)
		   )

;; (defun my/insert-eslint-disable-import-no-restricted-paths ()
;;   (interactive)
;;   (my/insert-line-before "/* eslint-disable import/no-restricted-paths */"))

;; (defun my/insert-eslint-enable-import-no-restricted-paths ()
;;   (interactive)
;;   (my/insert-line-after "/* eslint-enable import/no-restricted-paths */"))

;; (defun my/insert-eslint-disable-no-restricted-path-type-only ()
;;   (interactive)
;;   (my/insert-line-before "// eslint-disable-next-line import/no-restricted-paths -- type only import, doesn't affect bundle size"))

;; (bind-keys ("M-o" . my/insert-eslint-disable-import-no-restricted-paths)
;; 	   ("M-O" . my/insert-eslint-enable-import-no-restricted-paths)
;; 	   ("M-p" . my/insert-eslint-disable-no-restricted-path-type-only))

(defun my/refresh-magit-sentinel (process signal)
  (when (memq (process-status process) '(exit signal))
    (magit-refresh-all)
    (shell-command-sentinel process signal)))

(defun my/run-silent-and-refresh-magit (cmd)
  "Run CMD in a shell without displaying the output."
  ;; https://emacs.stackexchange.com/questions/42172/run-elisp-when-async-shell-command-is-done
  (let* ((buffer-name (generate-new-buffer  "*Yarn Async Shell Command*"))
		 (display-buffer-alist '(("*Yarn Async Shell Command*" display-buffer-no-window)))
		 (proc (progn

				 (async-shell-command cmd buffer-name)
				 (get-buffer-process buffer-name))
			   ))
    (if (process-live-p proc)
		(set-process-sentinel proc #'my/refresh-magit-sentinel)
      (magit-refresh-all))
    ))

(defun my/yarn-stage ()
  "See https://yarnpkg.com/cli/stage."
  (interactive)
  (my/run-silent-and-refresh-magit "yarn stage"))

(defun my/yarn-constraints-fix ()
  "See https://yarnpkg.com/cli/constraints."
  (interactive)
  (my/run-silent-and-refresh-magit "yarn constraints --fix"))

(defun my/yarn-install ()
  (interactive)
  (my/run-silent-and-refresh-magit "yarn install"))

;; bind to spc y
(bind-keys :map leader-map
		   ("y s" . my/yarn-stage)
		   ("y c" . my/yarn-constraints-fix)
		   ("y i" . my/yarn-install))

(use-package gdscript-mode
  :mode ("\\.gd\\'" . gdscript-ts-mode))

(envrc-global-mode)

(defun my/spawn-terminal (program name &optional directory)
  "Spawn a terminal with PROGRAM and NAME and return the buffer without displaying it."
  (let ((default-directory (or directory default-directory)))
	(term program)
	(rename-buffer name)
	(bury-buffer)
	(get-buffer name)))

(defun my/toggle-term (program name &optional directory)
  "Toggle a terminal with PROGRAM and NAME."
  (let* ((buffer (or (get-buffer name)
					 (my/spawn-terminal program name directory)))
		 (window (get-buffer-window name)))
	(if window
		(progn
		  (delete-window window)
		  (bury-buffer buffer))
	  (select-window (display-buffer buffer
									 '((display-buffer-in-side-window)
									   (side . bottom)
									   (window-height . 0.5)
									   (window-parameters
										(no-other-window . t)))))
	  (evil-insert-state))))

(defun my/toggle-term-in-project ()
  "Opens `term` in the current project root directory like vscode."
  (interactive)
  (my/toggle-term
   "fish"
   (format "*terminal %s*" (project-name (project-current)))
   (project-root (project-current))))

(defun my/toggle-term-in-workspace ()
  "Opens `term` in the current npm/pnpm/yarn workspace."
  (interactive)
  (when-let ((workspace (locate-dominating-file default-directory "package.json")))
	;; (message workspace)
	;; (message (file-name-nondirectory (directory-file-name workspace)))
	(my/toggle-term
	 "fish"
	 (format "*terminal %s*" (file-name-nondirectory (directory-file-name workspace)))
	 workspace)))

;; (file-name-nondirectory (directory-file-name (locate-dominating-file default-directory "package.json")))

(bind-keys ("C-`" . my/toggle-term-in-workspace))

(bind-keys ("M-H" . shrink-window-horizontally)
		   ("M-J" . shrink-window)
		   ("M-K" . enlarge-window)
		   ("M-L" . enlarge-window-horizontally))

(bind-keys :map evil-insert-state-map
		   ("M-H" . shrink-window-horizontally)
		   ("M-J" . shrink-window)
		   ("M-K" . enlarge-window)
		   ("M-L" . enlarge-window-horizontally))

(use-package tab-line
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  ;; (tab-line-tabs-function 'tab-line-tabs-buffer-groups)
  :config
  (defvar my/group-by-buffer-project-exclude-buffers-filter
	'(and (not "^ ")
		  (not "^*")
		  (not "^magit.*:")
		  (not (major-mode . dired-mode))))
  (defun my/group-by-buffer-project ()
	;; references among others: https://lists.gnu.org/archive/html/help-gnu-emacs/2020-08/msg00095.html
	"Group tabs by project."
	(let ((buffers (or (when-let ((project (project-current)))
						 (project-buffers project))
					   (buffer-list))))
	  (seq-sort-by #'buffer-name
				   #'string-lessp
				   (match-buffers
					my/group-by-buffer-project-exclude-buffers-filter
					buffers))))
  (setq tab-line-tabs-function 'my/group-by-buffer-project)
  (bind-keys
   ("C-<tab>" . tab-line-switch-to-next-tab)
   ("C-<iso-lefttab>" . tab-line-switch-to-prev-tab))
  (global-tab-line-mode))

(setopt
 ;; https://www.masteringemacs.org/article/demystifying-emacs-window-manager
 switch-to-buffer-obey-display-actions t
 switch-to-buffer-in-dedicated-window 'pop)

(bind-keys ("C-S-p" . window-toggle-side-windows))

(bind-keys
 ("M-E" . emoji-search))

(use-package hl-todo
  :config
  (global-hl-todo-mode))
