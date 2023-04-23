;; -*- lexical-binding: t -*-

(with-eval-after-load "apheleia"
  (setf (alist-get 'sort-package-json apheleia-formatters)
	'("sort-package-json" "--stdin")))

(defun apheleia-sort-package-json--is-package-json ()
  "Return t when the current buffer is visiting a package.json file."
  (and (not (equal (buffer-file-name) nil))
       (equal (file-name-nondirectory (buffer-file-name)) "package.json")))

(defun apheleia-sort-package-json--set-sort-package-json-formatter ()
  (when (apheleia-sort-package-json--is-package-json)
    (setq-local apheleia-formatter 'sort-package-json)))

(define-minor-mode apheleia-sort-package-json-minor-mode
  "Run sort-package-json on package.json files with apheleia."
  :global t
  :lighter " SortPkgJSON"
  (if apheleia-sort-package-json-minor-mode
      (add-hook 'json-ts-mode-hook 'apheleia-sort-package-json--set-sort-package-json-formatter 5)
    (remove-hook 'json-ts-mode-hook 'apheleia-sort-package-json--set-sort-package-json-formatter))
  (when (apheleia-sort-package-json--is-package-json)
    (run-hooks 'json-ts-mode-hook)))

(provide 'apheleia-sort-package-json)
