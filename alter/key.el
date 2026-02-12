;;; key.el --- Key bindings -*- lexical-binding: t; -*-
;;; Commentary:
;;;  Custom key mappings
;;; Code:

;; Unset defaults used for custom prefix maps
(global-unset-key "\C-e")
(global-unset-key "\C-a")
(global-unset-key "\C-z")
(global-unset-key "\C-l")
(global-unset-key "\C-m")
(global-unset-key "\M-m")
(global-unset-key "\M-o")
(global-unset-key "\C-r")

;; Provide Help alternative since C-h is rebound
;; F1 is generally safe across GUI/TTY
(global-set-key (kbd "<f1>") 'help-command)
;; Optional: C-? as another help access (may map to DEL on some terms)
(ignore-errors (global-set-key (kbd "C-?") 'help-command))

;; Use bind-key to namespace C-z prefix
(require 'bind-key)

;; Alt defaults
(global-set-key "\C-x\C-f" 'find-file)
(global-set-key "\C-s"     'consult-line)
(global-set-key "\C-r"     'consult-line)

;; Minimum
(global-set-key "\M-o" 'other-window)
(global-set-key "\C-@" 'other-window)
(global-set-key "\C-q" 'undo)
(global-set-key "\M-q" 'undo-redo)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'backward-kill-word)

;; Functional under C-z namespace
(bind-keys
 :prefix-map my/cz-map
 :prefix "C-z"
 ("C-r" . replace-string)
 ("C-p" . package-install)
 ("d"   . vc-diff)
 ("C-d" . vc-root-diff)
 ("C-s" . consult-git-grep)
 ("C-a" . consult-ripgrep))

;; project.el shortcuts under C-z p
(require 'project)
(bind-keys
 :prefix-map my/cz-project-map
 :prefix "C-z p"
 ("f" . project-find-file)
 ("p" . project-switch-project)
 ("g" . project-find-regexp))

;; Moving
(global-set-key "\C-e\C-c" 'shell)
(global-set-key "\C-e\C-m" 'consult-buffer)
(global-set-key "\M-n"     'next-buffer)
(global-set-key "\C-e\C-f" 'next-buffer)
(global-set-key "\M-p"     'previous-buffer)
(global-set-key "\C-e\C-b" 'previous-buffer)
(global-set-key "\C-e\C-a" 'move-beginning-of-line)
(global-set-key "\C-e\C-e" 'move-end-of-line)
(global-set-key "\C-e\C-l" 'browse-url)

;; Editorial
(global-set-key "\C-a\C-a" 'comment-dwim)
(global-set-key "\C-a\C-d" 'duplicate-thing)
(global-set-key "\C-a\C-r" 'rectangle-mark-mode)
(global-set-key "\C-a\C-k" 'kill-this-buffer)
(global-set-key "\C-a\C-y" 'consult-yank-pop)
(global-set-key "\C-a\C-q" 'quoted-insert)

;; Window change
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

(provide 'key)
;;; key.el ends here
