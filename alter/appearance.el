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
;; グローバルにシンタックスハイライトを有効化（冪等）
(global-font-lock-mode 1)
(add-hook 'after-init-hook (lambda () (global-font-lock-mode 1)))
;; major-mode 切替時にも確実に有効
(add-hook 'prog-mode-hook #'font-lock-mode)
(add-hook 'text-mode-hook #'font-lock-mode)
;; 装飾レベルは最大化（端末でもできる限り色分け）
(setq font-lock-maximum-decoration t)
;; 簡易デバッグ: 現在の font-lock 状態をレポート
(defun v5/debug-fontlock ()
  (interactive)
  (message "font-lock: mode=%s global=%s defaults=%s colors=%s gui=%s"
           font-lock-mode
           (bound-and-true-p global-font-lock-mode)
           font-lock-defaults
           (display-color-cells)
           (display-graphic-p)))
;; tree-sitter のハイライト詳細度を最大化
(when (boundp 'treesit-font-lock-level)
  (setq treesit-font-lock-level 4))

(provide 'appearance)
;;; appearance.el ends here
