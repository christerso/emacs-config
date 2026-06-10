;;; lang-asm.el --- x86-64 assembly support (NASM + GAS) -*- lexical-binding: t; -*-
;;; Commentary:
;; Two assembler dialects, two major modes:
;;   *.asm / *.nasm  -> `nasm-mode'  (https://github.com/skeeto/nasm-mode)
;;                      NASM/Intel syntax — what most x86-64 Linux tutorials use.
;;   *.s / *.S       -> built-in `asm-mode' (GAS/AT&T syntax, what gcc -S emits).
;;
;; `asm-lsp' (https://github.com/bergercookie/asm-lsp) is the language server
;; for both: hover docs for every instruction and register (ideal while
;; learning), completion, goto-def for labels.  Its global config lives in
;; ~/.config/asm-lsp/.asm-lsp.toml (x86-64, Linux).
;;
;; Build/run mirrors the other language modules: C-c c compile, C-c C-c
;; recompile, C-c r assemble+link+run the current file in *compilation*.
;; NASM:  nasm -f elf64 -g -F dwarf FILE.asm && ld FILE.o
;; GAS:   as -g FILE.s -o FILE.o && ld FILE.o   (or cc for libc programs)
;;; Code:

(eval-when-compile (require 'use-package))

(use-package nasm-mode
  :mode ("\\.\\(asm\\|nasm\\)\\'" . nasm-mode))

;; .s/.S already map to asm-mode by default; make it explicit for clarity.
(add-to-list 'auto-mode-alist '("\\.[sS]\\'" . asm-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((nasm-mode asm-mode) . ("asm-lsp"))))

(defun cs/asm--base ()
  "Current file name without directory or extension, shell-quoted."
  (shell-quote-argument (file-name-base buffer-file-name)))

(defun cs/asm--file ()
  "Current file name without directory, shell-quoted."
  (shell-quote-argument (file-name-nondirectory buffer-file-name)))

(defun cs/asm-setup ()
  "Common setup for assembly buffers (NASM and GAS)."
  (setq-local tab-width 8                  ;; column-aligned mnemonics/operands
              indent-tabs-mode t
              compile-command
              (cond ((file-exists-p "Makefile") "make ")
                    ((null buffer-file-name) "nasm -f elf64 ")
                    ((derived-mode-p 'nasm-mode)
                     (format "nasm -f elf64 -g -F dwarf %s -o %s.o && ld %s.o -o %s "
                             (cs/asm--file) (cs/asm--base) (cs/asm--base) (cs/asm--base)))
                    (t
                     (format "as -g %s -o %s.o && ld %s.o -o %s "
                             (cs/asm--file) (cs/asm--base) (cs/asm--base) (cs/asm--base)))))
  (when (executable-find "asm-lsp")
    (eglot-ensure)))

(add-hook 'nasm-mode-hook #'cs/asm-setup)
(add-hook 'asm-mode-hook  #'cs/asm-setup)

(defun cs/asm-run ()
  "Save, assemble, link, and run the current file in *compilation*."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (let ((base (cs/asm--base)))
    (compile (concat compile-command (format "&& ./%s" base)))))

(defun cs/asm-bind-keys (map)
  "Add the assembly build / run keys to keymap MAP."
  (define-key map (kbd "C-c c")   #'compile)
  (define-key map (kbd "C-c C-c") #'recompile)
  (define-key map (kbd "C-c r")   #'cs/asm-run))

(with-eval-after-load 'nasm-mode (cs/asm-bind-keys nasm-mode-map))
(with-eval-after-load 'asm-mode  (cs/asm-bind-keys asm-mode-map))

(provide 'lang-asm)
;;; lang-asm.el ends here
