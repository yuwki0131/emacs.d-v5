;;; primary.el --- Packages (primary) -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Core packages via package.el + package-vc
;;;  Vertico + Consult + Orderless + Marginalia + Embark
;;; Code:

(eval-when-compile (require 'use-package))
(require 'seq)
(require 'cl-lib)


;; Ensure core completion packages are installed (refresh archives if needed)
(when (and (not noninteractive) (>= emacs-major-version 29))
  (require 'package-vc nil t))

(defun v5/ensure-package (pkg &optional vc-url)
  "Install PKG if missing. If MELPA index is stale, refresh. Fallback to VC-URL when available."
  (unless (package-installed-p pkg)
    (condition-case _
        (progn
          (package-refresh-contents)
          (package-install pkg))
      (error
       (when (and vc-url (featurep 'package-vc))
         (ignore-errors
           (package-vc-install vc-url)))))))

(dolist (it '((vertico . "https://github.com/minad/vertico")
              (consult . "https://github.com/minad/consult")
              (marginalia . "https://github.com/minad/marginalia")
              (orderless . "https://github.com/oantolin/orderless")
              (embark . "https://github.com/oantolin/embark")
              (embark-consult . nil)))
  (v5/ensure-package (car it) (cdr it)))

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

;; Vertico (minibuffer UI)
(use-package vertico
  :init
  (vertico-mode 1)
  (setq vertico-resize t
        vertico-count 20))

;; Vertico-directory: ファイル名/パス編集中の削除挙動を改善
;; - Backspace/DEL/C-h で単語（ディレクトリ要素）単位で削除
;; - rfn-eshadow と連携してパスを整える
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-word)
              ("DEL"         . vertico-directory-delete-word)
              ("C-h"         . vertico-directory-delete-word)
              ("M-DEL"       . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult: commands on top of completion
(use-package consult
  :init
  ;; register/preview tweaks
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key (kbd ">")))

;; Embark: contextual actions
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

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
