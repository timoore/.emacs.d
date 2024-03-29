;;; From https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/

(require 'cl-lib)

(defvar lsp-packages
  '(lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company
             avy which-key helm-xref dap-mode))

(dolist (package lsp-packages)
  (cl-pushnew package package-selected-packages))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      ;; clangd is fast
      lsp-idle-delay 0.1
      lsp-clients-clangd-args '("-j=4" "-background-index" "-log=verbose" "--header-insertion-decorators=0"))

(setq lsp-keymap-prefix "s-c")

(setq lsp-completion-enable-additional-text-edit nil)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode)
  (setq lsp-completion-enable-additional-text-edit nil))
