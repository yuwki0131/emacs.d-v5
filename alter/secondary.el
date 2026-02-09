;;; secondary.el --- Packages (secondary) -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Convenience packages
;;; Code:

(eval-when-compile (require 'use-package))

(use-package duplicate-thing)

(use-package git-gutter
  :init (global-git-gutter-mode t))

;; Built-in electric-pair
(use-package elec-pair
  :ensure nil
  :config (electric-pair-mode +1))

(use-package dashboard
  :config
  (setq dashboard-center-content t)
  (dashboard-setup-startup-hook))

(provide 'secondary)
;;; secondary.el ends here
