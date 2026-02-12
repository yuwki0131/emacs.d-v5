;;; lang.el --- Language-specific -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Per-language settings and LSP (eglot) integration per project
;;;  Opt-in auto-connection for common languages.
;;; Code:

(eval-when-compile (require 'use-package))

;; Workaround for nix-mode packaging: nix-log.el uses helpers from nix-shell.el
;; Ensure the native compiler knows about these functions to avoid warnings.
(autoload 'nix-read-file "nix-shell")
(autoload 'nix-read-attr "nix-shell")
(autoload 'nix-read-flake "nix-shell")

;; -------- Eglot (LSP client) --------
(use-package eglot
  :commands (eglot eglot-ensure)
  :init
  ;; Be quiet and efficient by default
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0)
  ;; Increase IO for language servers
  (when (boundp 'read-process-output-max)
    (setq read-process-output-max (* 3 1024 1024)))
  :config
  ;; Helper: pick first existing executable from CANDIDATES
  (defun v5/first-exec (&rest candidates)
    (seq-find #'executable-find candidates))

  ;; Prefer modern servers when available; fall back gracefully
  (setq eglot-server-programs
        (append
         '(((yaml-mode yaml-ts-mode)
            . ("yaml-language-server" "--stdio"))
           ((json-mode json-ts-mode)
            . ("vscode-json-languageserver" "--stdio"))
           ((markdown-mode gfm-mode)
            . ("marksman" "server"))
           ((sh-mode bash-ts-mode)
            . ("bash-language-server" "start"))
           ((ruby-mode ruby-ts-mode)
            . ("ruby-lsp"))
           ((python-mode python-ts-mode)
            . ("basedpyright-langserver" "--stdio"))
           ((nix-mode nix-ts-mode)
            . ("nixd")))
         eglot-server-programs))

  ;; If preferred servers are missing, try alternates at runtime
  (defun v5/eglot-choose-server (mode)
    (pcase mode
      ((or 'yaml-mode 'yaml-ts-mode)
       (v5/first-exec "yaml-language-server"))
      ((or 'json-mode 'json-ts-mode)
       (or (v5/first-exec "vscode-json-languageserver" "vscode-json-languageserver-cli")
           (v5/first-exec "json-languageserver")))
      ((or 'markdown-mode 'gfm-mode)
       (v5/first-exec "marksman"))
      ((or 'sh-mode 'bash-ts-mode)
       (v5/first-exec "bash-language-server"))
      ((or 'ruby-mode 'ruby-ts-mode)
       (or (v5/first-exec "ruby-lsp") (v5/first-exec "solargraph")))
      ((or 'python-mode 'python-ts-mode)
       (or (v5/first-exec "basedpyright-langserver" "pyright-langserver")
           (v5/first-exec "pylsp")))
      ((or 'typescript-mode 'typescript-ts-mode 'tsx-ts-mode 'js-ts-mode)
       (v5/first-exec "typescript-language-server"))
      ((or 'nix-mode 'nix-ts-mode)
       (or (v5/first-exec "nixd") (v5/first-exec "nil")))
      (_ nil)))

  (defun v5/maybe-start-eglot ()
    "Start Eglot if a suitable server exists and in a project."
    (when (and (not (bound-and-true-p eglot--managed-mode))
               (or (ignore-errors (project-current)) t))
      (let* ((m major-mode)
             (srv (v5/eglot-choose-server m)))
        (when srv
          (eglot-ensure)))))
  )

;; -------- Language modes and hooks --------

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook ((yaml-mode . (lambda () (setq indent-tabs-mode nil yaml-indent-offset 2)))
         (yaml-mode . v5/maybe-start-eglot)))

;; JSON
(use-package json-mode
  :mode "\\.json\\'"
  :hook ((json-mode . v5/maybe-start-eglot)))

;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :hook ((markdown-mode . v5/maybe-start-eglot))
  :config (setq markdown-fontify-code-blocks-natively t))

;; Bash / Shell
(add-hook 'sh-mode-hook #'v5/maybe-start-eglot)
;; Dotfiles でも確実に sh-mode になるように関連付け
(dolist (rx '("/\\.bashrc\\'" "/\\.bash_profile\\'" "/\\.bash_aliases\\'" "/\\.profile\\'"))
  (add-to-list 'auto-mode-alist (cons rx 'sh-mode)))
;; .bashrc 等では bash 方言を既定に
(add-hook 'sh-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-match-p "/\\.bash\(rc\|_profile\|_aliases\)?\\'" buffer-file-name))
              (sh-set-shell "bash"))))

;; Ruby
(add-hook 'ruby-mode-hook #'v5/maybe-start-eglot)

;; Python
(add-hook 'python-mode-hook #'v5/maybe-start-eglot)

;; TypeScript / TSX / JS (tree-sitter if available)
(with-eval-after-load 'treesit
  (dolist (m '(typescript-ts-mode tsx-ts-mode js-ts-mode))
    (let ((hook (intern (format "%s-hook" m))))
      (add-hook hook #'v5/maybe-start-eglot))))

;; Legacy typescript-mode as fallback
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook ((typescript-mode . v5/maybe-start-eglot)))

;; Nix language
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode . v5/maybe-start-eglot)
         ;; so-long が長行検出で font-lock を無効化した場合に再有効化
         (nix-mode . (lambda ()
                       (unless font-lock-mode
                         (font-lock-mode 1))))))

(provide 'lang)
;;; lang.el ends here
