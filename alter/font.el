;;; font.el --- Font settings -*- lexical-binding: t; -*-
;;; Commentary:
;;;  基本フォントと絵文字/記号フォールバックの設定
;;; Code:

(global-font-lock-mode t)
(setq font-lock-support-mode 'jit-lock-mode)

(defvar default-font-name "HackGen35 Console")
(defvar default-font-jp-name "HackGen35 Console")

(when (and (display-graphic-p) (fboundp 'set-face-attribute))
  ;; 半角フォント: abc...
  (set-face-attribute 'default nil :family default-font-name :height 110)
  ;; 日本語全角フォント: あいうえお...
  (set-fontset-font 'nil 'japanese-jisx0208 (font-spec :family default-font-jp-name :height 105))
  ;; ギリシャ文字全角フォント: αβγκλ...
  (set-fontset-font 'nil '(#x0370 . #x03FF) (font-spec :family default-font-name :height 120))
  ;; キリル文字全角フォント: Эта статья ... Русский
  (set-fontset-font 'nil '(#x0400 . #x04FF) (font-spec :family default-font-name :height 120))

  ;; Emoji fallback (prefer Noto Color Emoji if available)
  (when (member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)
    ;; some emoji fall under 'symbol' script depending on build
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))

  ;; Nerd icons / all-the-icons fallbacks when fonts are present
  ;; Prefer them for symbol and Private Use Area (PUA) where icons live
  (dolist (fam '("Symbols Nerd Font Mono" "Symbols Nerd Font" "all-the-icons" "Material Icons" "FontAwesome" "Font Awesome 6 Free" "File Icons"))
    (when (member fam (font-family-list))
      (set-fontset-font t 'symbol (font-spec :family fam) nil 'append)
      (set-fontset-font t '(#xE000 . #xF8FF) (font-spec :family fam) nil 'append))
  ;; Generic symbol/Unicode fallbacks for common glyphs (e.g., ⚠ block elements)
  (dolist (fam '("Noto Sans Symbols" "Noto Sans Symbols2" "Noto Sans Symbols 2" "DejaVu Sans" "Symbola"))
    (when (member fam (font-family-list))
      (set-fontset-font t 'symbol (font-spec :family fam) nil 'append)
      (set-fontset-font t 'unicode (font-spec :family fam) nil 'append)
      (set-fontset-font t '(#x2580 . #x259F) (font-spec :family fam) nil 'append)))
)
  )


(defun v5/apply-font-fallbacks (&optional _frame)
  "Apply emoji and icon font fallbacks for the current frame."
  (when (and (display-graphic-p) (fboundp 'set-fontset-font))
    ;; Emoji fallback
    (when (member "Noto Color Emoji" (font-family-list))
      (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)
      (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'append))
    ;; Icons/PUA fallback
    (dolist (fam '("Symbols Nerd Font Mono" "Symbols Nerd Font" "all-the-icons" "Material Icons" "FontAwesome" "Font Awesome 6 Free" "File Icons"))
      (when (member fam (font-family-list))
        (set-fontset-font t 'symbol (font-spec :family fam) nil 'append)
        (set-fontset-font t '(#xE000 . #xF8FF) (font-spec :family fam) nil 'append)))))

(add-hook 'after-make-frame-functions #'v5/apply-font-fallbacks)
(add-hook 'emacs-startup-hook #'v5/apply-font-fallbacks)
(provide 'font)
;;; font.el ends here
