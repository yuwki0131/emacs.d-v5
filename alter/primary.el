;;; primary.el --- Packages (primary) -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Core packages via package.el + package-vc
;;; Code:

(eval-when-compile (require 'use-package))

;; counsel/ivy (M-x など)
(use-package counsel
  :bind (("M-x" . counsel-M-x))
  :config
  (setq ivy-height 30
        counsel-preselect-current-file t
        counsel-yank-pop-preselect-last t)
  ;; Ignore ./ and ../ in counsel-find-file
  (defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../"))))

;; swiper for isearch-like incremental search used by keybindings
(use-package swiper)

;; Marginalia: minibuffer annotations
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode 1))

;; Orderless: flexible completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Company: auto-completion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0
        company-tooltip-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-limit 20
        company-show-quick-access t))

;; Headerline: full path in header line
(use-package path-headerline-mode
  :config (path-headerline-mode +1))

;; Doom modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 22
        doom-modeline-lsp t)
  (doom-modeline-def-modeline 'my-simple-line
    '(bar input-method matches remote-host selection-info
          misc-info buffer-encoding process vcs buffer-info)
    '())
  (defun v5/setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my-simple-line 'default))
  (add-hook 'doom-modeline-mode-hook #'v5/setup-custom-doom-modeline))

;; Magit
(use-package magit)

;; Writable grep
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key "e"))

(provide 'primary)
;;; primary.el ends here
