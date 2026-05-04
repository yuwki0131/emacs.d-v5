;;; init.el --- User init -*- lexical-binding: t; -*-

;; Package archives (package.el)
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("melpa"  . "https://melpa.org/packages/")))

;; Initialize package.el once (early-init.el disables auto-init)
(package-initialize)

;; Prefer native compilation when available
(when (boundp 'package-native-compile)
  (setq package-native-compile t))

;; Upgrade built-in packages when asked
(setq package-install-upgrade-built-in t)

;; package-vc (Emacs 29+) integrates VCS installs with package.el
(when (>= emacs-major-version 29)
  (require 'package-vc))

;; Bootstrap use-package (for future modular config; ELPA-based)
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (ignore-errors (package-install 'use-package)))

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-verbose t)

;; Example: using package-vc via use-package
;; (use-package some-lib
;;   :vc (:url "https://github.com/user/some-lib" :branch "main"))

;; Load modular configs (ported from v4)
(add-to-list 'load-path (expand-file-name "alter" user-emacs-directory))

;; Load `custom.el` early so our modular configs can override faces/vars.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

(require 'font)
(require 'safety)
(require 'xolor)
(require 'builtin)
(require 'appearance)
(require 'primary)
(require 'secondary)
(require 'lang)
(require 'key)

(provide 'init)
;;; init.el ends here
