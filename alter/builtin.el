;;; builtin.el --- Core builtin behaviors -*- lexical-binding: t; -*-
;;; Commentary:
;;;  ビルトイン挙動の標準化（UTF-8/履歴/自動再読込等）
;;; Code:

;; 起動メッセージ等の抑制
(setq inhibit-startup-message t
      initial-scratch-message nil)

;; UTF-8 を優先
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)

;; y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

;; ミニバッファの改善
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; 常時トレイリングスペース削除（ユーザ指定）
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; recentf/savehist/save-place/auto-revert/so-long
(when (fboundp 'recentf-mode)
  (setq recentf-max-saved-items 300
        recentf-auto-cleanup 'never)
  (recentf-mode 1))
(make-directory (expand-file-name "var" user-emacs-directory) t)
(setq savehist-file (expand-file-name "var/history" user-emacs-directory))
(savehist-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
;; so-long: 長行での重さ対策。ただし通常ファイルで font-lock が落ちないよう閾値を上げる
(when (boundp 'so-long-threshold)
  (setq so-long-threshold 10000))
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode 1))

;; 何らかの理由で major-mode 切替時に font-lock が無効になっていたら復帰させる
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (unless font-lock-mode
              (font-lock-mode 1))
            (when (fboundp 'font-lock-ensure)
              (ignore-errors (font-lock-ensure)))))

;; 念のため明示的に有効化（環境によっては init 途中で無効化されることがある）
(setq font-lock-global-modes t)
(global-font-lock-mode 1)

;; バックアップ/オートセーブは不要（ユーザ指定）
(setq auto-save-default nil
      make-backup-files nil
      backup-inhibited t
      delete-auto-save-files t
      auto-save-list-file-name nil
      auto-save-list-file-prefix nil
      create-lockfiles nil)

;; サーバ起動
(require 'server)
(unless (server-running-p)
  (ignore-errors (server-start)))

(provide 'builtin)
;;; builtin.el ends here
