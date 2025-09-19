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

;; Force mode detection for recently opened files
(defun force-mode-detection ()
  "Force proper major mode detection for current buffer."
  (interactive)
  (when (buffer-file-name)
    (let ((filename (buffer-file-name)))
      (cond
       ;; Odin files
       ((string-match "\\.odin\\'" filename)
        (odin-mode)
        (message "Applied odin-mode to %s" (file-name-nondirectory filename)))
       ;; C files
       ((string-match "\\.\\(c\\|h\\)\\'" filename)
        (c-mode)
        (message "Applied c-mode to %s" (file-name-nondirectory filename)))
       ;; Go files
       ((string-match "\\.go\\'" filename)
        (go-mode)
        (message "Applied go-mode to %s" (file-name-nondirectory filename)))
       ;; Markdown files
       ((string-match "\\.\\(md\\|markdown\\)\\'" filename)
        (markdown-mode)
        (message "Applied markdown-mode to %s" (file-name-nondirectory filename)))
       ;; CMake files
       ((string-match "CMakeLists\\.txt\\'" filename)
        (cmake-mode)
        (message "Applied cmake-mode to %s" (file-name-nondirectory filename)))
       ((string-match "\\.cmake\\'" filename)
        (cmake-mode)
        (message "Applied cmake-mode to %s" (file-name-nondirectory filename)))
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

;; CMake support
(unless (package-installed-p 'cmake-mode)
  (package-install 'cmake-mode))

(when (package-installed-p 'cmake-mode)
  (require 'cmake-mode)
  ;; File associations for CMake
  (add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

  ;; Also handle Makefiles
  (add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-mode))
  (add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))

  (message "CMake and Makefile syntax highlighting configured"))

;; Configure ols for Odin (found in your repos!)
(let ((ols-path "G:/repos/ols/ols.exe"))  ; Your actual ols location
  (when (and ols-path (file-exists-p ols-path))
    (add-to-list 'eglot-server-programs `(odin-mode . (,ols-path)))
    (add-hook 'odin-mode-hook 'eglot-ensure)
    (message "ols configured at: %s" ols-path))
  (unless (and ols-path (file-exists-p ols-path))
    (message "ols not found - install from https://github.com/DanielGavin/ols")))

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
                          "C:/Program Files/MinGW-w64/mingw64/bin/gdb.exe")))
    (dolist (gdb-path gdb-locations)
      (when (executable-find gdb-path)
        (setq gud-gdb-command-name (concat gdb-path " -i=mi"))
        (message "Found GDB at: %s" gdb-path)
        (return)))))

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

;; Debugging keybindings
(global-set-key (kbd "C-c d d") 'debug-c-program)       ; Debug current C program
(global-set-key (kbd "C-c d r") 'debug-rift-project)    ; Debug rift project
(global-set-key (kbd "C-c d D") 'build-rift-debug)      ; Build debug version
(global-set-key (kbd "C-c d b") 'gdb-set-breakpoint-here) ; Set breakpoint
(global-set-key (kbd "C-c d n") 'gdb-step-over)         ; Step over (next)
(global-set-key (kbd "C-c d s") 'gdb-step-into)         ; Step into
(global-set-key (kbd "C-c d c") 'gdb-continue)          ; Continue
(global-set-key (kbd "C-c d g") 'gdb)                   ; Start GDB manually

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
