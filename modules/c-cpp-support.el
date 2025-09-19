;; C-specific settings
(add-hook 'c-mode-hook
          (lambda ()
            (setq c-default-style "k&r")
            (setq c-basic-offset 2)
            ;; Disable flymake to prevent conflicts with eglot
            (flymake-mode -1)
            ;; Use only eglot for xref, no etags
            (setq-local xref-backend-functions '(eglot-xref-backend))))

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

;; C++-specific settings
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-default-style "stroustrup")
            (setq c-basic-offset 4)
            ;; Disable flymake to prevent conflicts with eglot
            (flymake-mode -1)
            ;; Use only eglot for xref, no etags
            (setq-local xref-backend-functions '(eglot-xref-backend))))

;; File associations for C++
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.h\\+\\+\\'" . c++-mode))

;; Simple C++ build function
(defun build-cpp-file ()
  "Build current C++ file."
  (interactive)
  (when (eq major-mode 'c++-mode)
    (let ((current-file (buffer-file-name)))
      (when current-file
        (compile (format "clang++ -std=c++17 -o %s %s"
                        (file-name-sans-extension (file-name-nondirectory current-file))
                        (file-name-nondirectory current-file)))))))

;; Simple C++ run function
(defun run-cpp-file ()
  "Run current C++ executable."
  (interactive)
  (when (eq major-mode 'c++-mode)
    (let* ((current-file (buffer-file-name))
           (exe-name (when current-file
                       (file-name-sans-extension (file-name-nondirectory current-file)))))
      (when exe-name
        (if (file-exists-p (concat exe-name ".exe"))
            (async-shell-command (concat "./" exe-name ".exe") "*C++ Output*")
          (if (file-exists-p exe-name)
              (async-shell-command (concat "./" exe-name) "*C++ Output*")
            (message "No executable found. Build first with F3.")))))))

;; Disable flymake to prevent conflicts with eglot
(setq flymake-no-changes-timeout nil)
(setq flymake-start-on-flymake-mode nil)
(setq flymake-start-on-save-buffer nil)
(with-eval-after-load 'flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-cc))
(message "Flymake disabled to prevent eglot conflicts")

;; Auto-save all files before compiling
(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate t)

;; Navigate compilation errors with F8/Shift-F8
(global-set-key [f8] 'next-error)
(global-set-key [S-f8] 'previous-error)
(message "Auto-save on compile and error navigation configured")

;; Header/Implementation switcher - use ff-find-other-file (built-in)
(global-set-key (kbd "C-x z") 'ff-find-other-file)
(message "Header/implementation switcher (ff-find-other-file) configured (C-x z)")
