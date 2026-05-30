;;; lang-tsjs.el --- TypeScript / JavaScript support -*- lexical-binding: t; -*-
;;; Commentary:
;; Uses the Emacs 30 built-in tree-sitter modes (typescript-ts-mode,
;; tsx-ts-mode, js-ts-mode) — no external major-mode package needed.
;; `treesit-auto' (configured in config.org) handles the file->mode mapping and
;; prompts to install grammars on first open; the source recipes are registered
;; here so `M-x treesit-install-language-grammar' is reproducible after a wipe.
;;
;; `typescript-language-server'
;; (https://github.com/typescript-language-server/typescript-language-server)
;; drives both TS and JS via eglot.  Install with:
;;   npm install -g typescript typescript-language-server
;;
;; Format-on-save is gated on the server advertising a formatting capability.
;; For Prettier specifically, add `apheleia' later; the LSP formatter is fine
;; to start with.
;;; Code:

(with-eval-after-load 'treesit
  (dolist (src '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                 (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                 (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                 (json       "https://github.com/tree-sitter/tree-sitter-json")))
    (add-to-list 'treesit-language-source-alist src)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode)
                 . ("typescript-language-server" "--stdio"))))

(defun cs/tsjs-setup ()
  "Common setup for TypeScript / JavaScript buffers."
  (setq-local tab-width 2                   ;; JS/TS community convention
              js-indent-level 2
              typescript-ts-mode-indent-offset 2)
  (subword-mode 1)
  (when (executable-find "typescript-language-server")
    (eglot-ensure))
  (cs/eglot-format-on-save-if-capable))

(dolist (hook '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook))
  (add-hook hook #'cs/tsjs-setup))

(provide 'lang-tsjs)
;;; lang-tsjs.el ends here
