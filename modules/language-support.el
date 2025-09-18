;; C-specific settings
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "k&r")
            (setq c-basic-offset 2)))

;; Simple C build function
(defun build-c-file ()
  "Build current C file."
  (interactive)
  (when (eq major-mode 'c-mode)
    (let ((current-file (buffer-file-name)))
      (when current-file
        (compile (format "clang -std=c11 -o %s %s"
                        (file-name-sans-extension (file-name-nondirectory current-file))
                        (file-name-nondirectory current-file)))))))

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

;; Configure ols for Odin
(add-to-list 'eglot-server-programs '(odin-mode . ("ols")))
(add-hook 'odin-mode-hook 'eglot-ensure)

;; Debug: Check if odin-mode is available
(message "Odin mode available: %s" (if (fboundp 'odin-mode) "YES" "NO"))

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

;; Universal build function that detects language
(defun universal-build ()
  "Build current file/project based on language mode."
  (interactive)
  (cond
   ((eq major-mode 'odin-mode) (build-odin-file))
   ((eq major-mode 'c-mode) (build-c-file))
   (t (message "No build method for mode: %s" major-mode))))

(defun universal-run ()
  "Run current file/project based on language mode."
  (interactive)
  (cond
   ((eq major-mode 'odin-mode) (run-odin-file))
   (t (message "No run method for mode: %s" major-mode))))

;; Key bindings
(global-set-key [f3] 'universal-build)
(global-set-key [f4] 'universal-run)

(defun test-language-support-module ()
  "Test language support module."
  (interactive)
  (let ((results '()))

    ;; Test function definitions
    (dolist (func '(build-odin-file run-odin-file universal-build universal-run))
      (if (fboundp func)
          (push (format "%s: DEFINED" func) results)
        (push (format "%s: MISSING" func) results)))

    ;; Test modes
    (if (fboundp 'odin-mode)
        (push "odin-mode: AVAILABLE" results)
      (push "odin-mode: MISSING" results))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Language Support Test*")
      (erase-buffer)
      (insert "=== Language Support Module Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (display-buffer (current-buffer)))

    (message "Language support test completed")))
