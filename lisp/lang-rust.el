;;; lang-rust.el --- Rust language support -*- lexical-binding: t; -*-
;;; Commentary:
;; `rust-ts-mode' (built into Emacs 29+) for the major mode with the rust
;; tree-sitter grammar, and `rust-analyzer' (from rustup, on
;; /usr/lib/rustup/bin) as the language server via eglot.  Mirrors the
;; Neovim side (rustaceanvim + clippy-on-save) as closely as eglot allows:
;; clippy as the check command, all cargo features, and rustfmt on save via
;; the LSP formatter.
;;
;; Build/run/test use the generic `compile' workflow like the Go and C
;; modules: C-c c compile, C-c C-c recompile, C-c r cargo run,
;; C-c t t cargo test, C-c t c cargo clippy.
;;; Code:

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) . ("rust-analyzer")))
  ;; Server settings, matching the Neovim rustaceanvim config.
  (setq-default eglot-workspace-configuration
                (append eglot-workspace-configuration
                        '(:rust-analyzer
                          (:cargo (:allFeatures t :buildScripts (:enable t))
                           :checkOnSave t
                           :check (:command "clippy" :extraArgs ["--no-deps"])
                           :procMacro (:enable t))))))

(defun cs/rust-setup ()
  "Common setup for Rust buffers."
  (setq-local indent-tabs-mode nil          ;; rustfmt: spaces
              tab-width 4
              rust-ts-mode-indent-offset 4
              compile-command (if (locate-dominating-file default-directory "Cargo.toml")
                                  "cargo build "
                                "rustc "))
  (subword-mode 1)
  (when (executable-find "rust-analyzer")
    (eglot-ensure))
  ;; rust-analyzer advertises formatting (it shells out to rustfmt).
  (cs/eglot-format-on-save-if-capable))

(add-hook 'rust-ts-mode-hook #'cs/rust-setup)

(defun cs/rust-run ()
  "Save and `cargo run' (or `rustc && run' outside a crate) in *compilation*."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (if (locate-dominating-file default-directory "Cargo.toml")
      (compile "cargo run")
    (let ((file (shell-quote-argument (file-name-nondirectory buffer-file-name)))
          (bin  (shell-quote-argument (file-name-base buffer-file-name))))
      (compile (format "rustc %s -o %s && ./%s" file bin bin)))))

(with-eval-after-load 'rust-ts-mode
  (define-key rust-ts-mode-map (kbd "C-c c")   #'compile)
  (define-key rust-ts-mode-map (kbd "C-c C-c") #'recompile)
  (define-key rust-ts-mode-map (kbd "C-c r")   #'cs/rust-run)
  (define-key rust-ts-mode-map (kbd "C-c t t")
              (lambda () (interactive)
                (when (buffer-modified-p) (save-buffer))
                (compile "cargo test")))
  (define-key rust-ts-mode-map (kbd "C-c t c")
              (lambda () (interactive)
                (when (buffer-modified-p) (save-buffer))
                (compile "cargo clippy"))))

(provide 'lang-rust)
;;; lang-rust.el ends here
