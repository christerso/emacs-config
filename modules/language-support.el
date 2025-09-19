;; Force mode detection for recently opened files
(defun force-mode-detection ()
  "Force proper major mode detection for current buffer."
  (interactive)
  (when (buffer-file-name)
    (let ((filename (buffer-file-name)))
      (cond
       ;; Odin files
       ((string-match "\\.odin\\'" filename)
        (when (fboundp 'odin-mode)
          (odin-mode)
          (message "Applied odin-mode to %s" (file-name-nondirectory filename))))
       ;; C files
       ((string-match "\\.\\(c\\|h\\)\\'" filename)
        (c-mode)
        (message "Applied c-mode to %s" (file-name-nondirectory filename)))
       ;; Go files
       ((string-match "\\.go\\'" filename)
        (when (fboundp 'go-mode)
          (go-mode)
          (message "Applied go-mode to %s" (file-name-nondirectory filename))))
       ;; Markdown files
       ((string-match "\\.\\(md\\|markdown\\)\\'" filename)
        (when (fboundp 'markdown-mode)
          (markdown-mode)
          (message "Applied markdown-mode to %s" (file-name-nondirectory filename))))
       ;; CMake files
       ((string-match "CMakeLists\\.txt\\'" filename)
        (when (fboundp 'cmake-mode)
          (cmake-mode)
          (message "Applied cmake-mode to %s" (file-name-nondirectory filename))))
       ((string-match "\\.cmake\\'" filename)
        (when (fboundp 'cmake-mode)
          (cmake-mode)
          (message "Applied cmake-mode to %s" (file-name-nondirectory filename))))
       ;; Makefiles
       ((string-match "[Mm]akefile\\'" filename)
        (makefile-mode)
        (message "Applied makefile-mode to %s" (file-name-nondirectory filename)))
       (t
        (set-auto-mode)
        (message "Applied auto-mode to %s" (file-name-nondirectory filename)))))))

;; Hook to ensure mode is applied when files are opened
(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (eq major-mode 'fundamental-mode)) ; File didn't get proper mode
              (force-mode-detection))))

;; Global key to manually fix mode
(global-set-key (kbd "C-c m") 'force-mode-detection)

;; Optional language modes - install if available but don't fail if missing
(when (not (package-installed-p 'go-mode))
  (condition-case err
      (package-install 'go-mode)
    (error
     (message "Could not install go-mode: %s" (error-message-string err)))))

(when (not (package-installed-p 'markdown-mode))
  (condition-case err
      (package-install 'markdown-mode)
    (error
     (message "Could not install markdown-mode: %s" (error-message-string err)))))

;; CMake support
(condition-case err
    (progn
      (unless (package-installed-p 'cmake-mode)
        (package-install 'cmake-mode))
      (require 'cmake-mode)
      ;; File associations for CMake
      (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
      (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

      ;; Also handle Makefiles
      (add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-mode))
      (add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))

      (message "CMake and Makefile syntax highlighting configured"))
  (error
   (message "Could not configure CMake support: %s" (error-message-string err))))

;; Debug: Check if odin-mode is available
(message "Odin mode available: %s" (if (fboundp 'odin-mode) "YES" "NO"))

;; Universal build function that detects language
(defun universal-build ()
  "Build current file/project based on language mode."
  (interactive)
  (cond
   ((eq major-mode 'odin-mode) (when (fboundp 'build-odin-file) (build-odin-file)))
   ((eq major-mode 'c-mode) (when (fboundp 'build-c-file) (build-c-file)))
   ((eq major-mode 'c++-mode) (when (fboundp 'build-cpp-file) (build-cpp-file)))
   (t (message "No build method for mode: %s" major-mode))))

(defun universal-run ()
  "Run current file/project based on language mode."
  (interactive)
  (cond
   ((eq major-mode 'odin-mode) (when (fboundp 'run-odin-file) (run-odin-file)))
   ((eq major-mode 'c++-mode) (when (fboundp 'run-cpp-file) (run-cpp-file)))
   (t (message "No run method for mode: %s" major-mode))))

;; Key bindings
(global-set-key [f3] 'universal-build)
;; Note: F4 is handled by project-builds module for smart project running

;; GDB Debugging Configuration for Windows
;; Configure GDB for Windows with MinGW/MSYS2

;; GDB configuration
(setq gdb-many-windows t)                    ; Enable multiple debugging windows
(setq gdb-show-main t)                       ; Show main source window
(setq gud-gdb-command-name "gdb -i=mi")      ; Use GDB machine interface

;; Windows-specific GDB setup
(when (eq system-type 'windows-nt)
  ;; Try different GDB locations on Windows
  (let ((gdb-locations '("gdb"
                          "C:/msys64/mingw64/bin/gdb.exe"
                          "C:/mingw64/bin/gdb.exe"
                          "C:/Program Files/MinGW-w64/mingw64/bin/gdb.exe"))
        (gdb-found nil))
    (dolist (gdb-path gdb-locations)
      (when (and (not gdb-found) (executable-find gdb-path))
        (setq gud-gdb-command-name (concat gdb-path " -i=mi"))
        (message "Found GDB at: %s" gdb-path)
        (setq gdb-found t)))))

;; Enhanced debugging functions for C projects
(defun debug-c-program ()
  "Debug current C program with GDB in Emacs."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (exe-name (if current-file
                       (file-name-sans-extension (file-name-nondirectory current-file))
                     "a")))
    (if (file-exists-p (concat exe-name ".exe"))
        (gdb (concat gud-gdb-command-name " " exe-name ".exe"))
      (if (file-exists-p exe-name)
          (gdb (concat gud-gdb-command-name " " exe-name))
        (message "No executable found. Build first with F3.")))))

(defun build-rift-debug ()
  "Build rift project in Debug configuration."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (compile "cmake --build build --config Debug")))

(defun debug-rift-project ()
  "Debug rift project with GDB using Debug build."
  (interactive)
  (let* ((default-directory "G:/repos/rift/")
         (debug-exe-path "build/Debug/rift.exe")
         (release-exe-path "build/Release/rift.exe"))
    (cond
     ((file-exists-p debug-exe-path)
      (gdb (concat gud-gdb-command-name " " debug-exe-path)))
     ((file-exists-p release-exe-path)
      (message "Using Release build (no debug symbols). Run: cmake --build build --config Debug")
      (gdb (concat gud-gdb-command-name " " release-exe-path)))
     (t
      (message "No rift executable found. Build first with: cmake --build build --config Debug")))))

;; Quick debugging commands
(defun gdb-set-breakpoint-here ()
  "Set GDB breakpoint at current line."
  (interactive)
  (if (bound-and-true-p gud-minor-mode)
      (gud-break 1)
    (message "Not in a GDB session. Start debugging first.")))

(defun gdb-step-over ()
  "GDB step over (next line)."
  (interactive)
  (if (bound-and-true-p gud-minor-mode)
      (gud-next 1)
    (message "Not in a GDB session. Start debugging first.")))

(defun gdb-step-into ()
  "GDB step into function."
  (interactive)
  (if (bound-and-true-p gud-minor-mode)
      (gud-step 1)
    (message "Not in a GDB session. Start debugging first.")))

(defun gdb-continue ()
  "GDB continue execution."
  (interactive)
  (if (bound-and-true-p gud-minor-mode)
      (gud-cont 1)
    (message "Not in a GDB session. Start debugging first.")))

;; Debugging keybindings - create prefix keymap first
(defvar debug-keymap (make-sparse-keymap)
  "Keymap for debugging commands.")

(define-key debug-keymap (kbd "d") 'debug-c-program)       ; Debug current C program
(define-key debug-keymap (kbd "r") 'debug-rift-project)    ; Debug rift project
(define-key debug-keymap (kbd "D") 'build-rift-debug)      ; Build debug version
(define-key debug-keymap (kbd "b") 'gdb-set-breakpoint-here) ; Set breakpoint
(define-key debug-keymap (kbd "n") 'gdb-step-over)         ; Step over (next)
(define-key debug-keymap (kbd "s") 'gdb-step-into)         ; Step into
(define-key debug-keymap (kbd "c") 'gdb-continue)          ; Continue
(define-key debug-keymap (kbd "g") 'gdb)                   ; Start GDB manually

(global-set-key (kbd "C-c d") debug-keymap)

;; F5 for quick debugging (alternative to RemedyBG)
(global-set-key [f5] 'debug-rift-project)

(message "GDB debugging configured for Windows")

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
