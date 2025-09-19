(defun smart-build-with-libraries ()
  "Build project with library dependencies, ensuring single executable output."
  (interactive)
  (let* ((project-root (or (condition-case nil (projectile-project-root) (error nil))
                          default-directory))
         (build-config (expand-file-name "build.odin" project-root))
         (libs-dir (expand-file-name "libs" project-root)))

    (message "Multi-stage build starting in: %s" project-root)

    (cond
     ;; Custom build script exists
     ((file-exists-p build-config)
      (let ((default-directory project-root))
        (compile "odin run build.odin -file")))

     ;; Project with libraries directory
     ((and (file-directory-p libs-dir) (directory-files libs-dir nil "\\.odin$"))
      (let ((default-directory project-root)
            (project-name (file-name-nondirectory (directory-file-name project-root))))
        ;; Build libraries first, then main executable
        (compile (format "odin build . -out:%s -opt:2 -no-bounds-check" project-name))))

     ;; Standard project build
     (t
      (smart-build)))))

(defun smart-build-release ()
  "Build optimized release version as single executable."
  (interactive)
  (let* ((project-root (or (condition-case nil (projectile-project-root) (error nil))
                          default-directory))
         (project-name (file-name-nondirectory (directory-file-name project-root))))

    (message "Building release version: %s" project-name)

    (cond
     ;; Odin release build
     ((directory-files project-root nil "\\.odin$")
      (let ((default-directory project-root))
        (compile (format "odin build . -out:%s -opt:3 -no-bounds-check -subsystem:console" project-name))))

     ;; C/C++ release build
     ((file-exists-p (expand-file-name "CMakeLists.txt" project-root))
      (let ((default-directory project-root))
        (compile "cmake --build build --config Release")))

     ;; Go release build
     ((file-exists-p (expand-file-name "go.mod" project-root))
      (let ((default-directory project-root))
        (compile "go build -ldflags='-s -w' -o main .")))

     ;; Rust release build
     ((file-exists-p (expand-file-name "Cargo.toml" project-root))
      (let ((default-directory project-root))
        (compile "cargo build --release")))

     (t
      (message "No recognized project type for release build")))))

(defun test-enhanced-build-module ()
  "Test the enhanced build system module functions."
  (interactive)
  (let ((test-results '()))

    ;; Test function definitions
    (dolist (func '(smart-build-with-libraries smart-build-release))
      (if (fboundp func)
          (push (format "%s: DEFINED" func) test-results)
        (push (format "%s: MISSING" func) test-results)))

    ;; Test project detection
    (let ((project-root (or (condition-case nil (projectile-project-root) (error nil))
                           default-directory)))
      (push (format "Project root: %s" project-root) test-results)

      ;; Test file detection
      (let ((odin-files (directory-files project-root nil "\\.odin$"))
            (cmake-file (file-exists-p (expand-file-name "CMakeLists.txt" project-root)))
            (go-mod (file-exists-p (expand-file-name "go.mod" project-root))))
        (push (format "Odin files: %d" (length odin-files)) test-results)
        (push (format "CMake: %s" (if cmake-file "YES" "NO")) test-results)
        (push (format "Go module: %s" (if go-mod "YES" "NO")) test-results)))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Enhanced Build Test Results*")
      (erase-buffer)
      (insert "=== Enhanced Build Module Test Results ===\n\n")
      (dolist (result (reverse test-results))
        (insert (format "%s\n" result)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))

    (message "Enhanced build module test completed")))

(defun rift-build ()
  "Build the rift project using CMake."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (compile "cmake --build build --config Release")))

(defun rift-run ()
  "Run the rift executable."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (shell-command "build\\\\Release\\\\rift.exe")))

(defun rift-debug ()
  "Launch rift in RemedyBG debugger."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (shell-command "C:/remedybg.exe rift.rdbg")))

;; F3: Build rift project
(global-set-key [f3] 'rift-build)

;; F4: Run rift executable
(global-set-key [f4] 'rift-run)

;; F5: Build debug version
(defun rift-build-debug ()
  "Build rift project in Debug configuration."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (compile "cmake --build build --config Debug")))

(global-set-key [f5] 'rift-build-debug)

;; Shift+F4: Launch in RemedyBG
(global-set-key [S-f4] 'rift-debug)
