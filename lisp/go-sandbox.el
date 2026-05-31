;;; go-sandbox.el --- Jump to a persistent Go scratchpad -*- lexical-binding: t; -*-
;;; Commentary:
;; `C-c m s' opens ~/go-sandbox/main.go — a real Go module (so gopls, eglot,
;; completion and format-on-save all work, unlike a bare scratch buffer) kept
;; around for trying snippets. Run it with `C-c r' (cs/go-run-file, the Go
;; build/run key) or `C-c c' to compile.
;;
;; The module is created on first open if it does not exist, so the sandbox
;; survives `rm -rf'. A prefix argument (`C-u C-c m s') resets the file back to
;; the starter template.
;;; Code:

(defvar cs/go-sandbox-dir (expand-file-name "~/go-sandbox")
  "Directory holding the persistent Go sandbox module.")

(defconst cs/go-sandbox-template
  "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"sandbox ready\")\n}\n"
  "Starter contents for a fresh sandbox `main.go'.")

(defun cs/go-sandbox (&optional reset)
  "Open the persistent Go sandbox at `cs/go-sandbox-dir'/main.go.
Create the module (go.mod + main.go) if it is missing.  With a prefix argument
RESET, overwrite main.go with the starter template first."
  (interactive "P")
  (let ((main (expand-file-name "main.go" cs/go-sandbox-dir))
        (mod  (expand-file-name "go.mod"  cs/go-sandbox-dir)))
    (unless (file-directory-p cs/go-sandbox-dir)
      (make-directory cs/go-sandbox-dir t))
    (unless (file-exists-p mod)
      (with-temp-file mod (insert "module go-sandbox\n\ngo 1.26\n")))
    (when (or reset (not (file-exists-p main)))
      ;; If the file is open, reset its buffer too, not just the file on disk.
      (let ((buf (get-file-buffer main)))
        (with-temp-file main (insert cs/go-sandbox-template))
        (when buf
          (with-current-buffer buf (revert-buffer t t t)))))
    (find-file main)))

(global-set-key (kbd "C-c m s") #'cs/go-sandbox)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c m s" "go-sandbox"))

(provide 'go-sandbox)
;;; go-sandbox.el ends here
