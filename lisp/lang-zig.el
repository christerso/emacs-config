;;; lang-zig.el --- Zig language support -*- lexical-binding: t; -*-
;;; Commentary:
;; `zig-mode' (https://github.com/ziglang/zig-mode) for the major mode and
;; `zls' (https://github.com/zigtools/zls) as the language server via eglot.
;;
;; Formatting is handled by zig-mode's built-in `zig fmt' on save — it is the
;; canonical Zig formatter and needs no server, so we use it instead of the
;; LSP formatter.  Build/run use the generic `compile' workflow.
;;; Code:

(eval-when-compile (require 'use-package))

(use-package zig-mode
  :mode "\\.zig\\'"
  :custom
  (zig-format-on-save t)          ;; canonical `zig fmt' on save
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((zig-mode) . ("zls"))))

  (defun cs/zig-setup ()
    "Common setup for Zig buffers."
    (setq-local compile-command
                (if (file-exists-p "build.zig") "zig build " "zig run "))
    (subword-mode 1)
    (when (executable-find "zls")
      (eglot-ensure)))
  (add-hook 'zig-mode-hook #'cs/zig-setup)

  (defun cs/zig-run ()
    "Save and run the current Zig file or project in *compilation*."
    (interactive)
    (when (buffer-modified-p) (save-buffer))
    (compile (if (file-exists-p "build.zig")
                 "zig build run "
               (concat "zig run "
                       (shell-quote-argument
                        (file-name-nondirectory buffer-file-name))))))

  (define-key zig-mode-map (kbd "C-c c")   #'compile)
  (define-key zig-mode-map (kbd "C-c C-c") #'recompile)
  (define-key zig-mode-map (kbd "C-c r")   #'cs/zig-run)
  (define-key zig-mode-map (kbd "C-c t")
              (lambda () (interactive)
                (when (buffer-modified-p) (save-buffer))
                (compile "zig test "))))

(provide 'lang-zig)
;;; lang-zig.el ends here
