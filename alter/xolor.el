;;; xolor.el --- Color theme tweaks -*- lexical-binding: t; -*-
;;; Commentary:
;;;  v4 の色設定を移植。`list-faces-display` で確認できます。
;;; Code:

(require 'color)

;; Palette
(defvar v/main          "#0997B6")   ;; darkcyan
(defvar v/secondary     "#FF4C00")   ;; orange
(defvar v/emphasis      "#E5266A")   ;; deep pink
(defvar v/emphasis/weak "#FC9CBF")   ;; weak deep pink
(defvar v/string-like   "#3CB371")   ;; green

;; Grayscale
(defvar v/foreground     "#101010")
(defvar v/background/inv "#333333")
(defvar v/out-strong     "#666666")
(defvar v/out            "#777777")
(defvar v/foreground/inv "#E0E0E0")
(defvar v/focus          "#F0F0F0")
(defvar v/background     "#FFFFFF")

;; Base foreground/background
(set-foreground-color v/foreground)
(set-background-color v/background)

;; Helpers
(defun set-face-app1 (attr-symbol color-name bold italic)
  (set-face-foreground attr-symbol color-name)
  (set-face-bold       attr-symbol bold)
  (set-face-italic     attr-symbol italic))

(defun set-face-app2 (attr-symbol color-fg color-bg bold italic)
  (set-face-foreground attr-symbol color-fg)
  (set-face-background attr-symbol color-bg)
  (set-face-bold       attr-symbol bold)
  (set-face-italic     attr-symbol italic))

;; Syntax highlighting
(set-face-app1 'font-lock-comment-delimiter-face v/out-strong  t   t)
(set-face-app1 'font-lock-comment-face           v/out         nil t)
(set-face-app1 'font-lock-doc-face               v/emphasis    nil t)
(set-face-app1 'font-lock-string-face            v/string-like t   nil)
(set-face-app1 'font-lock-keyword-face           v/main        t   nil)
(set-face-app1 'font-lock-builtin-face           v/main        nil nil)
(set-face-app1 'font-lock-function-name-face     v/secondary   t   nil)
(set-face-app1 'font-lock-variable-name-face     v/secondary   nil nil)
(set-face-app1 'font-lock-type-face              v/main        t   nil)
(set-face-app1 'font-lock-constant-face          v/emphasis    t   nil)
(set-face-app1 'font-lock-warning-face           v/emphasis    nil t)
(set-face-app1 'font-lock-preprocessor-face      v/main        nil nil)
(set-face-app1 'font-lock-negation-char-face     v/main        nil nil)

;; Region
(set-face-attribute 'region nil :background v/emphasis/weak)

;; Current line
(custom-set-faces `(hl-line ((t (:background ,v/focus)))))

;; Cursor
(set-cursor-color v/emphasis)

;; Mode-line
(custom-set-faces
 `(mode-line
   ((t (:background ,v/focus :foreground ,v/foreground :box nil))))
 `(mode-line-inactive
   ((t (:background ,v/background :foreground ,v/out
        :box (:line-width 1 :color ,v/focus :style nil))))))

;; Header-line
(custom-set-faces
 `(header-line
   ((t (:box (:line-width (4 . 4) :color ,v/background/inv)
        :foreground ,v/foreground/inv
        :background ,v/background/inv)))))

;; Minibuffer
(custom-set-faces `(minibuffer-prompt ((t (:foreground ,v/main :weight bold)))))

;; git-gutter
(custom-set-faces
 `(git-gutter:added   ((t (:foreground ,v/main          :background ,v/main))))
 `(git-gutter:deleted ((t (:foreground ,v/secondary     :background ,v/secondary))))
 `(git-gutter:modified((t (:foreground ,v/emphasis/weak :background ,v/emphasis/weak)))))

;; line numbers
(custom-set-faces
 `(line-number              ((t (:foreground ,v/out))))
 `(line-number-current-line ((t (:foreground ,v/foreground)))))

;; Vertico/Orderless/Consult/Marginalia/Embark
(custom-set-faces
 `(vertico-current        ((t (:background ,v/emphasis/weak :foreground ,v/main))))
 `(vertico-group-title    ((t (:foreground ,v/out))))
 `(vertico-group-separator((t (:foreground ,v/out))))
 `(vertico-directory      ((t (:foreground ,v/main))))
 `(orderless-match-face-0 ((t (:foreground ,v/main        :weight bold))))
 `(orderless-match-face-1 ((t (:foreground ,v/secondary   :weight bold))))
 `(orderless-match-face-2 ((t (:foreground ,v/emphasis    :weight bold))))
 `(orderless-match-face-3 ((t (:foreground ,v/string-like :weight bold))))
 `(consult-preview-match  ((t (:background ,v/emphasis/weak))))
 `(consult-highlight      ((t (:foreground ,v/emphasis    :weight bold))))
 `(marginalia-documentation ((t (:foreground ,v/out))))
 `(marginalia-key           ((t (:foreground ,v/main))))
 `(marginalia-value         ((t (:foreground ,v/secondary))))
 `(embark-keybinding        ((t (:foreground ,v/secondary))))
 `(embark-verbose-indicator-title ((t (:foreground ,v/main :weight bold)))))

;; Doom-modeline alignment to base palette
(custom-set-faces
 `(doom-modeline-buffer-file        ((t (:foreground ,v/foreground))))
 `(doom-modeline-buffer-modified    ((t (:foreground ,v/secondary :weight bold))))
 `(doom-modeline-info               ((t (:foreground ,v/main))))
 `(doom-modeline-warning            ((t (:foreground ,v/emphasis))))
 `(doom-modeline-project-dir        ((t (:foreground ,v/main))))
 `(doom-modeline-buffer-major-mode  ((t (:foreground ,v/main :weight bold))))
 `(doom-modeline-urgent             ((t (:foreground ,v/emphasis)))))

;; counsel/ivy
(custom-set-faces
 `(ivy-current-match
   ((t (:background ,v/emphasis/weak :foreground ,v/main :underline nil))))
 `(ivy-minibuffer-match-face-1 ((t (:foreground ,v/secondary))))
 `(ivy-minibuffer-match-face-2 ((t (:foreground ,v/emphasis))))
 `(ivy-minibuffer-match-face-3 ((t (:foreground ,v/main))))
 `(ivy-minibuffer-match-face-4 ((t (:foreground ,v/string-like))))
 `(ivy-subdir ((t (:foreground ,v/main))))
 '(ivy-highlight-face ((t (:foreground "magenta")))))

;; company
(custom-set-faces
 `(company-tooltip       ((t (:foreground ,v/foreground/inv :background ,v/background/inv))))
 `(company-scrollbar-bg  ((t (:background ,v/background/inv))))
 `(company-scrollbar-fg  ((t (:background ,v/foreground/inv))))
 `(company-tooltip-selection ((t (:foreground ,v/secondary :background ,v/focus))))
 `(company-tooltip-common ((t (:foreground ,v/emphasis))))
 `(company-preview       ((t (:background ,v/background/inv :foreground ,v/foreground/inv))))
 `(company-preview-common ((t (:foreground ,v/foreground/inv)))))

;; swiper/ivy
(custom-set-faces
 `(swiper-match-face-2
   ((t (:background ,v/emphasis/weak :foreground ,v/foreground :weight bold)))))

(provide 'xolor)
;;; xolor.el ends here
