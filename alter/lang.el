;;; lang.el --- Language-specific -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Per-language settings
;;; Code:

(eval-when-compile (require 'use-package))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . (lambda ()
                       (setq indent-tabs-mode nil
                             yaml-indent-offset 2))))

(provide 'lang)
;;; lang.el ends here
