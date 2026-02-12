;;; appearance.el --- Appearance settings -*- lexical-binding: t; -*-
;;; Commentary:
;;;  見た目全般（行番号/フリンジ/スクロール/タイトルなど）
;;; Code:

;; カーソルタイプ（GUIのみ）
(when (display-graphic-p)
  (setq-default cursor-type '(bar . 3)))

;; フレームタイトル（プロジェクト名 + バッファ名）
(setq frame-title-format
      '(:eval (let* ((proj (when (fboundp 'project-current)
                              (when-let ((p (project-current)))
                                (file-name-nondirectory (directory-file-name (project-root p))))))
                      (buf  (buffer-name)))
               (if proj (format "%s - %s" proj buf) buf))))

;; カーソル行ハイライト
(global-hl-line-mode t)

;; フリンジは0（ユーザ指定）
(when (fboundp 'set-fringe-mode)
  (set-fringe-mode 0))

;; 行番号はグローバル（ユーザ指定）
(when (fboundp 'global-display-line-numbers-mode)
  (setq-default display-line-numbers 'absolute)
  (global-display-line-numbers-mode 1))

;; スクロール体験の改善
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(setq scroll-conservatively 101
      scroll-preserve-screen-position t)

;; 対応する括弧のハイライト / 選択の上書き
(show-paren-mode 1)
(delete-selection-mode 1)
;; グローバルにシンタックスハイライトを有効化
(global-font-lock-mode 1)

(provide 'appearance)
;;; appearance.el ends here
