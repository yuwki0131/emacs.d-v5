;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Disable automatic package.el initialization at startup
(setq package-enable-at-startup nil)

;; Reduce UI flicker by disabling UI elements as early as possible
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Startup performance tweaks
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t
      site-run-file nil)

;; Prefer newer .el over .elc
(setq load-prefer-newer t)

;; Silence native-comp warnings during async compile
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Restore GC after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.2)))

;;; early-init.el ends here
