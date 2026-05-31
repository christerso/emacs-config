;;; init.el --- Minimal bootstrap; the real config lives in config.org -*- lexical-binding: t; -*-

;; Package archives — required before config.org can install anything.
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)

;; Keep `M-x customize` from rewriting init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Load the literate core configuration.
(let ((conf (expand-file-name "config.org" user-emacs-directory)))
  (when (file-exists-p conf)
    (require 'org)
    (org-babel-load-file conf)))

;; Load the modular pieces (vendored modes + per-language support + search).
;; Plain .el so they load directly — no per-file org tangling. Order matters:
;; quick-search and the lang modules depend on helpers/packages from config.org.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(dolist (module '("quick-search"
                  "session-layout"
                  "run-console"
                  "bottom-terminal"
                  "lang-odin"
                  "lang-zig"
                  "lang-sql"
                  "lang-tsjs"
                  "lang-formats"
                  "lang-markdown"
                  "json-fix"))
  (load module nil 'nomessage))

;;; init.el ends here
