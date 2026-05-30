;;; lang-sql.el --- SQL support -*- lexical-binding: t; -*-
;;; Commentary:
;; Built-in `sql-mode' plus `sql-indent' (https://github.com/alex-hhh/emacs-sql-indent)
;; for reliable indentation.  Dialect defaults to PostgreSQL; switch per-buffer
;; with `M-x sql-set-product'.
;;
;; LSP via `sqls' (https://github.com/sqls-server/sqls) is OPTIONAL — sqls needs
;; a connection config (~/.config/sqls/config.yml), so eglot only starts when
;; the binary is present.  Formatting goes through whichever external formatter
;; is installed: pgFormatter (`pg_format') or sqlparse (`sqlformat').
;;; Code:

(eval-when-compile (require 'use-package))

(use-package sql
  :ensure nil
  :mode ("\\.sql\\'" . sql-mode)
  :custom
  (sql-product 'postgres)
  :config
  (add-hook 'sql-mode-hook
            (lambda ()
              (setq-local tab-width 4)
              (when (and (executable-find "sqls") (fboundp 'eglot-ensure))
                (eglot-ensure)))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls"))))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

(defun cs/sql-format-buffer ()
  "Format the SQL buffer with pg_format or sqlformat, whichever is installed."
  (interactive)
  (let ((fmt (cond ((executable-find "pg_format") "pg_format -")
                   ((executable-find "sqlformat")
                    "sqlformat --reindent --keywords upper --identifiers lower -")
                   (t nil))))
    (unless fmt
      (user-error "No SQL formatter found (install pgformatter or python-sqlparse)"))
    (let ((p (point)))
      (shell-command-on-region (point-min) (point-max) fmt nil t)
      (goto-char (min p (point-max))))))

(with-eval-after-load 'sql
  ;; C-c C-f (not C-c f) so the global find/search prefix stays intact in SQL.
  (define-key sql-mode-map (kbd "C-c C-f") #'cs/sql-format-buffer))

(provide 'lang-sql)
;;; lang-sql.el ends here
