(recentf-mode)

(setq leader-map (make-sparse-keymap)) ;; bind SPC-* keybindings here

(defun my/find-init-file ()
  "Open user-init-file."
  (interactive)
  (find-file "~/.config/nixpkgs/emacs/init.el"))

(defun my/find-home-manager-config ()
  "Open home manager config file."
  (interactive)
  (find-file "~/.config/nixpkgs/home.nix"))

(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package magit)

(use-package apheleia
  :config
  (apheleia-global-mode)
  (add-to-list 'apheleia-mode-alist '(emacs-lisp-mode . lisp-indent)))

(use-package nix-mode)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  (nix-mode . eglot-ensure))

(use-package evil
  :init
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil) ;; required by evil-collection
  :custom
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-indent-plus
  :after evil
  :bind (
	 :map evil-inner-text-objects-map
	 ("i" . evil-indent-plus-i-indent)
	 ("I" . evil-indent-plus-i-indent-up)
	 ("k" . evil-indent-plus-i-indent-up)
	 ("j" . evil-indent-plus-i-indent-up-down)
	 ("J" . evil-indent-plus-i-indent-up-down)
	 :map evil-outer-text-objects-map
	 ("i" . evil-indent-plus-a-indent)
	 ("I" . evil-indent-plus-a-indent-up)
	 ("J" . evil-indent-plus-a-indent-up-down)))

(define-key leader-map (kbd "f") 'find-file)
(define-key leader-map (kbd "g") 'magit-status)
(define-key leader-map (kbd "p") project-prefix-map)
(define-key leader-map (kbd "d") 'my/find-init-file)
(define-key leader-map (kbd "b") 'switch-to-buffer)
(define-key leader-map (kbd "SPC") 'execute-extended-command)

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

(with-eval-after-load 'evil
  (my/setup-evil-leader-key))

(require 'treesit)
