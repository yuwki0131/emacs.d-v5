# emacs.d-v5

概要
- 目的: v4 を踏襲しつつ、よりモダンで扱いやすい Emacs 構成を提供。
- パッケージ管理: package.el + package-vc（Emacs 29+）を採用。`early-init.el` で自動初期化を抑止し、`init.el` で一括初期化。
- 補完/検索: Vertico + Consult + Orderless + Marginalia + Embark に統一（既存の C-s/M-x 等は互換割当）。
- LSP/プロジェクト: `eglot` を標準採用し、`project.el` と連携。yaml/json/markdown/bash/ruby/python/typescript/nix を opt-in で自動接続。
- 表示: fringe=0、行番号はグローバル有効、トレイリングスペースは常時削除、バックアップ/オートセーブ無効。
- アイコン/絵文字: Nerd Icons（必要に応じて自動有効化）と emoji フォントの存在を検査し、なければ豆腐回避のため無効化。

主な構成ファイル
- `early-init.el`: package.el の自動起動を止め、描画/GC を最適化。
- `init.el`: 依存の初期化、`alter/` 以下のモジュール読込。
- `alter/` ディレクトリ: 機能別の設定群。
  - `primary.el`: Vertico/Consult/…、doom-modeline、company 等のコアパッケージ。
  - `key.el`: 既存運指に沿ったキーバインド、C-z プレフィクス、project ショートカット。
  - `appearance.el`: fringe/行番号/スクロール等の見た目設定。
  - `builtin.el`: UTF-8、履歴、auto-revert、バックアップ無効化などの基本挙動。
  - `xolor.el`: v4 の配色移植（font-lock/region/mode-line など）。
  - `font.el`: フォント系（Nerd/emoji の有無診断、起動後の有効化ガード）。
  - `lang.el`: 各言語モードと Eglot の自動接続フック。
  - `safety.el`: face への nil 設定ガード、ELPA バイトコンパイル時のノイズ警告抑制。

起動方法（.emacs.d を指定）
- Emacs 29 以降: `emacs --init-directory /home/yuwki0131/myconfig/emacs.d-v5`
- それ以前: `emacs -q --load /home/yuwki0131/myconfig/emacs.d-v5/init.el`


## NixOS 設定（LSP/フォントの推奨）
この v5 は「実行ファイルが PATH にあれば対象モードで Eglot を起動」します。以下を `configuration.nix` に追記してください。

```nix
environment.systemPackages = with pkgs; [
  # LSP servers
  yaml-language-server
  vscode-langservers-extracted     # json/html/css など（json は consult 側設定に互換）
  marksman                         # Markdown
  bash-language-server
  ruby-lsp                         # 代替: (rubyPackages.solargraph)
  basedpyright                     # 代替: pyright / python3Packages.python-lsp-server
  nodePackages.typescript          # tsserver
  nodePackages.typescript-language-server
  nixd                             # Nix（代替: nil）

  # 補助ツール（任意）
  ripgrep fd fzf git
];

# フォント（アイコン/絵文字）。サイズが許容できれば nerdfonts を丸ごと導入。
# 個別指定したい場合は環境に合わせて調整してください。
fonts = {
  enableDefaultPackages = true;
  packages = with pkgs; [
    nerdfonts                 # Nerd Fonts（重い場合は必要なフォントだけに絞る）
    noto-fonts-emoji          # 絵文字
  ];
};

# フォントキャッシュ更新（必要であれば）
system.activationScripts.updateFontCache.text = ''
  if command -v fc-cache >/dev/null; then fc-cache -f -v || true; fi
'';
```

備考
- `ruby-lsp` はチャンネルによって `ruby-lsp` か `(rubyPackages.ruby-lsp)` で提供されます。
- JSON サーバは `vscode-langservers-extracted` に含まれる `vscode-json-languageserver` を利用します。
- `$HOME/.bashrc` などホーム直下のファイルは通常プロジェクト外のため、自動では Eglot を起動しません。必要なら `M-x eglot` で手動接続してください。


## macOS 準備手順
前提: Homebrew 利用（https://brew.sh/）。

1) Emacs の用意（29+ 推奨）
- `brew install emacs` もしくは GUI が必要なら `brew install --cask emacs`（emacs-plus を使う場合は各自の好みで）。

2) LSP サーバ類
```bash
brew install yaml-language-server marksman bash-language-server ruby-lsp pyright basedpyright \
  node typescript typescript-language-server nixd
brew install vscode-langservers-extracted
```

補足:
- JSON 用には `brew install vscode-langservers-extracted`（もしくは Node 環境で `npm i -g vscode-langservers-extracted`）。
- Python は `basedpyright` 推奨ですが、`pyright` や `python-lsp-server` でも可。
- Nix LSP は `nixd` 推奨（代替: `nil`）。

3) フォント（アイコン/絵文字）
- Nerd Icons のために Nerd Font をインストール：
  - `brew tap homebrew/cask-fonts`
  - `brew install --cask font-symbols-only-nerd-font`（軽量） あるいはお好みの Nerd Font を導入
- 絵文字:
  - `brew install --cask font-noto-color-emoji`
- Emacs 上でも `M-x nerd-icons-install-fonts` / `M-x all-the-icons-install-fonts` を実行可能（システムフォントの導入が安定）。

4) 起動
- Emacs 29+:
  - `emacs --init-directory /path/to/emacs.d-v5`
- 初回起動後、必要に応じて M-x で `eglot` を起動し、各サーバが正しく接続されるか確認してください。

トラブルシュート
- 豆腐（□）が出る: Nerd Font / emoji フォントの導入とフォントキャッシュ更新を確認。GUI でログインしなおすと反映が早い場合があります。
- Vertico のファイル名編集で単語削除: `<backspace>/C-h/DEL/M-DEL` に `vertico-directory-delete-word` を割当済み。
- `.bashrc` など: `sh-mode` で開き、bash 方言を既定に。LSP はプロジェクト外なので通常は手動 `M-x eglot`。
- バイトコンパイルの警告が多い: ELPA 再コンパイル時のみ bytecomp 警告を抑制するガードを導入済み。通常起動時の警告は隠しません。

ライセンス
- このリポジトリの設定ファイルは、特段の明記が無い限り利用者の責任でご自由にお使いください。
