;;; lang-odin.el --- Odin language support -*- lexical-binding: t; -*-
;;; Commentary:
;; Vendored `odin-mode' (lisp/odin-mode.el) as the major mode, with `ols'
;; (https://github.com/DanielGavin/ols) as the language server via eglot.
;;
;; Build/run use the generic `compile' workflow with portable,
;; project-relative commands — no hardcoded paths.  `compile-command' is set
;; per buffer: `make' if a Makefile exists, else `odin build src' for the
;; common src/-layout project, else `odin run .' for a flat package.
;;
;; Format-on-save is gated on the server actually advertising a formatting
;; capability (`cs/eglot-format-on-save-if-capable', defined in config.org),
;; so it never errors when `odinfmt' is absent.
;;; Code:

(require 'odin-mode)
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(odin-mode . ("ols"))))

(defun cs/odin-setup ()
  "Common setup for Odin buffers."
  (setq-local indent-tabs-mode t            ;; Odin convention: tabs
              tab-width 4
              compile-command
              (cond ((file-exists-p "Makefile") "make ")
                    ((file-directory-p "src")   "odin build src ")
                    (t                          "odin run . ")))
  (subword-mode 1)
  (when (executable-find "ols")
    (eglot-ensure))
  (cs/eglot-format-on-save-if-capable))

(add-hook 'odin-mode-hook #'cs/odin-setup)

(defun cs/odin-run ()
  "Save and `odin run' the current package directory in *compilation*."
  (interactive)
  (when (buffer-modified-p) (save-buffer))
  (compile (if (file-directory-p "src") "odin run src " "odin run . ")))

(with-eval-after-load 'odin-mode
  (define-key odin-mode-map (kbd "C-c c")   #'compile)     ;; edit build cmd
  (define-key odin-mode-map (kbd "C-c C-c") #'recompile)   ;; re-run last
  (define-key odin-mode-map (kbd "C-c r")   #'cs/odin-run)); run package

(provide 'lang-odin)
;;; lang-odin.el ends here
