;;; primary.el --- Packages (primary) -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Core packages via package.el + package-vc
;;; Code:

(eval-when-compile (require 'use-package))
(require 'seq)
(require 'cl-lib)

(defvar v5/force-disable-icons nil 
  "If non-nil, force-disable icons to avoid tofu.")

(defun v5/icon-sample-codes ()
  "Return a small set of common Nerd icon codepoints."
  '(#xF101 #xF0A0 #xF120 #xF0E7 #xEA60))

(defun v5/icon-glyphs-displayable-p ()
  "Return non-nil if sample PUA glyphs render."
  (when (display-graphic-p)
    (cl-every (lambda (cp) (char-displayable-p (decode-char 'ucs cp)))
              (v5/icon-sample-codes))))


;; Icons for modeline and UI
(use-package all-the-icons
  :if (display-graphic-p)
  :defer t)

;; Nerd icons for doom-modeline (preferred by recent versions)
(defvar nerd-icons-font-family "Symbols Nerd Font Mono")
(use-package nerd-icons
  :if (display-graphic-p)
  :demand t
  :config
  (setq nerd-icons-font-family (or nerd-icons-font-family "Symbols Nerd Font Mono")))

(defun v5/icon-fonts-installed-p ()
  "Return non-nil if any icon font family seems available."
  (let* ((families (font-family-list))
         (names '("Symbols Nerd Font Mono" "Symbols Nerd Font"
                  "all-the-icons" "Material Icons" "FontAwesome"
                  "octicons" "Weather Icons" "File Icons")))
    (seq-some (lambda (n) (member n families)) names)))

(defun v5/ensure-icon-fonts (&optional _frame)
  "Ensure icon fonts are installed for the current GUI frame."
  (when (display-graphic-p)
    (unless (v5/icon-fonts-installed-p)
      ;; Try installing Nerd and All-the-icons fonts
      (ignore-errors (when (require 'nerd-icons nil t)
                       (nerd-icons-install-fonts t)))
      (ignore-errors (when (require 'all-the-icons nil t)
                       (all-the-icons-install-fonts t)))
      ;; Refresh font cache if available
      (when (executable-find "fc-cache")
        (ignore-errors (call-process "fc-cache" nil nil nil "-f" "-v"))))))

;; Run once at startup and also when creating first GUI frame (daemon use)
(add-hook 'emacs-startup-hook #'v5/ensure-icon-fonts)
(add-hook 'after-make-frame-functions #'v5/ensure-icon-fonts)

(defun v5/diagnose-icons ()
  "Report icon/emoji font availability."
  (interactive)
  (message "Icons installed: %s, Emoji present: %s"
           (if (v5/icon-fonts-installed-p) 'yes 'no)
           (member "Noto Color Emoji" (font-family-list))))

;; Toggle icons after fonts become available
(defun v5/enable-icons-if-ready (&optional _frame)
  (when (and (display-graphic-p)
             (v5/icon-fonts-installed-p)
             (v5/icon-glyphs-displayable-p)
             (boundp 'doom-modeline-icon))
    (setq doom-modeline-icon t)
    ;; set nerd-icons font family to a sane default if present
    (when (boundp 'nerd-icons-font-family)
      (let ((fam (or (car (seq-filter (lambda (n) (member n (font-family-list)))
                                      '("Symbols Nerd Font Mono" "Symbols Nerd Font")))
                     nerd-icons-font-family)))
        (setq nerd-icons-font-family fam)))
    (when (fboundp 'doom-modeline-refresh) (doom-modeline-refresh))))

(add-hook 'emacs-startup-hook #'v5/enable-icons-if-ready)
(add-hook 'after-make-frame-functions #'v5/enable-icons-if-ready)

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

;; Headerline: show path only for real files to avoid nil errors
(use-package path-headerline-mode
  :commands path-headerline-mode
  :init
  (defun v5/enable-path-headerline-if-file ()
    (when buffer-file-name
      (path-headerline-mode +1)))
  :hook ((find-file . v5/enable-path-headerline-if-file))
  :config
  ;; Be explicit: disable in non-file/special buffers
  (add-hook 'after-change-major-mode-hook
            (lambda ()
              (unless buffer-file-name
                (when (bound-and-true-p path-headerline-mode)
                  (path-headerline-mode -1)))))
  (with-eval-after-load 'dashboard
    (add-hook 'dashboard-mode-hook
              (lambda ()
                (when (bound-and-true-p path-headerline-mode)
                  (path-headerline-mode -1))))))

;; Doom modeline
(use-package doom-modeline
  :after nerd-icons
  :init
  (setq doom-modeline-height 22
        doom-modeline-lsp t
        doom-modeline-icon nil
        doom-modeline-unicode-fallback t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-github nil
        doom-modeline-github-interval 0
        doom-modeline-buffer-encoding nil
        doom-modeline-modal-icon nil
        doom-modeline-env-version nil
        doom-modeline-mu4e nil
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-battery nil
        doom-modeline-time nil
        doom-modeline-checker-simple-format nil
        doom-modeline-vcs-max-length 12)
  :config
  ;; Guard battery updates when icon toggles, to avoid stringp nil errors
  (defun v5/doom-battery-guard (orig &rest args)
    (when (and (boundp 'doom-modeline-battery)
               doom-modeline-battery
               (ignore-errors (battery)))
      (apply orig args)))
  (advice-add 'doom-modeline-update-battery-status :around #'v5/doom-battery-guard)
  :hook (after-init . doom-modeline-mode))

;; Magit
(use-package magit)

;; Writable grep
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t
        wgrep-enable-key "e"))

(provide 'primary)
;;; primary.el ends here
