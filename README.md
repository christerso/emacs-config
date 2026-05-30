# emacs-config

Christer's Emacs configuration. A clean, literate core ([`config.org`](config.org))
on top of modern Emacs 30 built-ins — [eglot](https://github.com/joaotavora/eglot)
for LSP, [treesit](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html)
for syntax, [dape](https://github.com/svaante/dape) for debugging — with per-language
support split into small, single-purpose modules under [`lisp/`](lisp/).

## Layout

```
init.el            Minimal bootstrap: package archives, loads config.org, then lisp/*
config.org         Literate core: defaults, completion stack, eglot, Go, C/C++, Org
lisp/
  quick-search.el  Telescope-style fuzzy "search everything" (C-c f …)
  lang-odin.el     Odin   — odin-mode + ols
  lang-zig.el      Zig    — zig-mode + zls
  lang-sql.el      SQL    — sql-mode + sql-indent (+ optional sqls)
  lang-tsjs.el     TS/JS  — built-in ts-modes + typescript-language-server
  odin-mode.el     Vendored Odin major mode (glassofethanol/odin-mode)
themes/            voidlight theme
```

Each language module is self-contained, defined exactly once, and uses the
generic `compile`/`recompile` workflow (no hardcoded project paths).

## Completion / search

[vertico](https://github.com/minad/vertico) + [orderless](https://github.com/oantolin/orderless)
+ [marginalia](https://github.com/minad/marginalia) + [consult](https://github.com/minad/consult)
+ [embark](https://github.com/oantolin/embark), with in-buffer
[corfu](https://github.com/minad/corfu)/[cape](https://github.com/minad/cape).
Minibuffer icons via [nerd-icons-completion](https://github.com/rainstormstudio/nerd-icons-completion).

### Search keys (`C-c f …`)

| Key | Action | LazyVim analogue |
|-----|--------|------------------|
| `C-c f p` | find file in project | `<leader>ff` |
| `C-c f d` | fast file find (fd) anywhere | `<leader>ff` |
| `C-c f g` | live grep project (ripgrep) | `<leader>sg` |
| `C-c f l` | search lines in buffer | `<leader>sb` |
| `C-c f b` / `C-x b` | switch buffer (preview) | `<leader>,` |
| `C-c f r` | recent files | `<leader>fr` |
| `C-c f j` / `f i` | symbols in file / project | `<leader>ss` |
| `C-c f o` | outline / headings | |
| `C-c f e` | diagnostics | `<leader>xx` |

## Build / run keys (per language)

`C-c c` compile · `C-c C-c` recompile · `C-c r` run-this-file/package.
Go adds `C-c t <k>` tests; C/C++ adds `C-c o` source↔header; Zig adds `C-c t` test;
SQL uses `C-c C-f` to format.

## Requirements

- Emacs 30+
- Language servers (install what you use):
  - Go: `gopls`, `dlv`
  - C/C++: `clangd`
  - Odin: [`ols`](https://github.com/DanielGavin/ols)
  - Zig: [`zls`](https://github.com/zigtools/zls) + `zig`
  - TS/JS: `npm install -g typescript typescript-language-server`
  - SQL (optional): [`sqls`](https://github.com/sqls-server/sqls); formatter `pg_format` or `sqlformat`
- `ripgrep` and `fd` for search
- Run `M-x nerd-icons-install-fonts` once

## Install

```bash
git clone https://github.com/christerso/emacs-config ~/.emacs.d
```

First start installs packages and prompts to build tree-sitter grammars on demand.
