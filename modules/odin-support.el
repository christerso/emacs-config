;; Download odin-mode if needed
(let ((odin-mode-dir (expand-file-name "odin-mode" user-emacs-directory))
      (odin-mode-file (expand-file-name "odin-mode/odin-mode.el" user-emacs-directory)))
  (unless (file-exists-p odin-mode-file)
    (make-directory odin-mode-dir t)
    (url-copy-file "https://raw.githubusercontent.com/mattt-b/odin-mode/master/odin-mode.el"
                   odin-mode-file t))
  (load-file odin-mode-file)
  (require 'odin-mode))

;; Configure file associations for Odin
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

;; Odin-specific settings with xref support
(add-hook 'odin-mode-hook
          (lambda ()
            ;; Enable xref for Odin files
            (setq-local xref-backend-functions '(etags--xref-backend))
            (message "Odin mode loaded with xref support")))

;; Configure ols for Odin (found in your repos!)
(let ((ols-path "G:/repos/ols/ols.exe"))
  (when (and ols-path (file-exists-p ols-path))
    ;; Check if eglot is available before configuring
    (when (featurep 'eglot)
      (add-to-list 'eglot-server-programs `(odin-mode . (,ols-path)))
      (add-hook 'odin-mode-hook 'eglot-ensure)
      (message "ols configured at: %s" ols-path))
    (unless (featurep 'eglot)
      (message "eglot not loaded - ols LSP support disabled")))
  (unless (and ols-path (file-exists-p ols-path))
    (message "ols not found - install from https://github.com/DanielGavin/ols")))

;; Simple Odin build function
(defun build-odin-file ()
  "Build current Odin file or project."
  (interactive)
  (when (eq major-mode 'odin-mode)
    (let* ((project-root default-directory)
           (odin-files (directory-files project-root nil "\\.odin$"))
           (project-name (file-name-nondirectory (directory-file-name project-root))))
      (if (> (length odin-files) 1)
          (compile (format "odin build . -out:%s" project-name))
        (let ((current-file (buffer-file-name)))
          (when current-file
            (compile (format "odin build %s -file -out:%s"
                            (file-name-nondirectory current-file)
                            (file-name-sans-extension (file-name-nondirectory current-file))))))))))

;; Simple Odin run function
(defun run-odin-file ()
  "Run current Odin file or project."
  (interactive)
  (when (eq major-mode 'odin-mode)
    (let* ((project-root default-directory)
           (odin-files (directory-files project-root nil "\\.odin$"))
           (project-name (file-name-nondirectory (directory-file-name project-root))))
      (if (> (length odin-files) 1)
          (async-shell-command (format "odin run . -out:%s" project-name) "*Odin Output*")
        (let ((current-file (buffer-file-name)))
          (when current-file
            (async-shell-command (format "odin run %s -file -out:%s"
                                        (file-name-nondirectory current-file)
                                        (file-name-sans-extension (file-name-nondirectory current-file)))
                                "*Odin Output*")))))))

(message "Odin development support configured")
