;;; lang-formats.el --- Serialization / schema file formats -*- lexical-binding: t; -*-
;;; Commentary:
;; Highlighting for schema / IDL formats that have NO built-in tree-sitter mode
;; in Emacs 30, so they are vendored as plain major modes in lisp/:
;;
;;   * Protocol Buffers (.proto) — `protobuf-mode', vendored from
;;     https://github.com/protocolbuffers/protobuf (editors/protobuf-mode.el).
;;   * Cap'n Proto (.capnp) — `capnp-mode', vendored from
;;     https://github.com/capnproto/capnproto (highlighting/emacs/capnp-mode.el).
;;
;; The serialization/config formats that DO ship a tree-sitter mode — YAML,
;; Bash, TOML, Dockerfile, CMake — are handled by their built-in `*-ts-mode'
;; (grammars registered + built in config.org, modes remapped by treesit-auto),
;; so they need nothing here.
;;; Code:

(require 'protobuf-mode)
(require 'capnp-mode)

;; `protobuf-mode' registers its `.proto' auto-mode entry behind an autoload
;; cookie, which never fires for a vendored `require'; register it explicitly.
;; (`capnp-mode' adds its own `.capnp' entry at load, so it needs no help.)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

(provide 'lang-formats)
;;; lang-formats.el ends here
