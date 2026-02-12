# Repository Guidelines

## Project Structure & Module Organization
- Root: `early-init.el`, `init.el`, `custom.el`（自動生成）。
- `alter/`: 機能別モジュール（編集対象の中心）。
  - `primary.el`（Vertico/Consult/Embark 等）、`key.el`、`appearance.el`、`builtin.el`、`xolor.el`、`font.el`、`lang.el`（Eglot/言語）、`safety.el`、`secondary.el`。
- 生成物・環境依存: `elpa/`, `eln-cache/`, `var/`, `auto-save-list/`, `recentf`, `places`, `history`（コミットしない）。

## Build, Test, and Development Commands
- 起動検証（バッチ）：
  - `emacs --batch --init-directory "$PWD" --eval '(message "batch-load-ok")'`
- 再コンパイル（ELPA）：
  - `emacs --batch --init-directory "$PWD" --eval '(require \='package) (package-initialize) (package-recompile-all)'`
- アイコン/絵文字診断：`M-x v5/diagnose-icons`

## Coding Style & Naming Conventions
- Emacs Lisp、`-*- lexical-binding: t; -*-` を先頭に。`use-package` を用いて宣言的に設定。
- パッケージ管理は `package.el + package-vc`。VC 由来は `:vc` を使用。
- 関数/変数の接頭辞は `v5/`。副作用はフックで最小化。OS/GUI 依存は `:if`/`display-graphic-p` で分岐。
- Face 属性に `nil` を渡さない（必要なら `'unspecified`）。

## Testing Guidelines
- 追加のユニットテストは不要。変更時は必ずバッチ起動と ELPA 再コンパイルで回帰確認。
- 主要モード（yaml/json/md/sh/ruby/python/ts/nix）を開いて Eglot が起動するかを手動確認。
- 大きな変更ではスクリーンショット（モデルライン/絵文字表示）を添付。

## Commit & Pull Request Guidelines
- 小さく焦点の合ったコミットに分割。件名は「領域: 要約」（例: `alter/lang: add nixd to eglot`）。
- PR には概要、変更点、動作確認手順、環境（NixOS/macOS/Windows）と既知の制約を記載。
- 生成物はコミットしない（`elpa/`, `eln-cache/`, `var/`, `recentf`, `history` など）。`.gitignore` を維持。

## Security & Configuration Tips
- 認証情報や環境固有パスを `custom.el` 等に書かない。必要なツールは README の手順で導入。
- フォント/アイコンは存在チェックの上で有効化。無い環境では自動的に無効化される設計を維持。

