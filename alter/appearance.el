;;; appearance.el --- Appearance settings -*- lexical-binding: t; -*-
;;; Commentary:
;;;  見た目全般（行番号/フリンジ/スクロール/タイトルなど）
;;; Code:

;; カーソルタイプ（GUIのみ）
;; Prefer a thin bar cursor. Keep it compatible with TTY by using the symbol
;; form (some terminals ignore pixel-width settings like (bar . N)).
(setq-default cursor-type 'bar)
(setq cursor-type 'bar)
(when (display-graphic-p)
  (setq x-stretch-cursor nil))

;; 検索中もカーソル形状を変えない（環境によって box になるのを防ぐ）
(add-hook 'isearch-mode-hook
          (lambda ()
            (setq cursor-type 'bar)))
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq cursor-type 'bar)))

;; フレームタイトル（プロジェクト名 + バッファ名）
(setq frame-title-format
      '(:eval (let* ((proj (when (fboundp 'project-current)
                              (when-let ((p (project-current)))
                                (file-name-nondirectory (directory-file-name (project-root p))))))
                      (buf  (buffer-name)))
               (if proj (format "%s - %s" proj buf) buf))))


;; 行間
(setq-default line-spacing 3)

;; カーソル行ハイライト
(setq hl-line-sticky-flag t)
;; Global-Hl-Line はデフォルトだと「選択ウィンドウのみ」なので、
;; 検索時に minibuffer を選択するとバッファ側のハイライトが消えます。
;; これを防ぐため、グローバル版も sticky にします。
(setq global-hl-line-sticky-flag t)
(global-hl-line-mode t)
;; Minibuffer はハイライトしない（入力行の背景色で見づらくなるため）
(defun v5/minibuffer-disable-hl-line ()
  "Disable `hl-line-mode' in the minibuffer only."
  (when (bound-and-true-p hl-line-mode)
    (hl-line-mode -1)))
(add-hook 'minibuffer-setup-hook #'v5/minibuffer-disable-hl-line)

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
