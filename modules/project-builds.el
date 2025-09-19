(defun build-rift ()
  "Build rift C project with CMake."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (compile "cmake --build build --config Release")))

(defun build-rift-debug ()
  "Build rift C project with CMake debug."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (compile "cmake --build build --config Debug")))

(defun run-rift ()
  "Run rift executable."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (async-shell-command "build\\Release\\rift.exe" "*Rift Output*")))

(defun debug-rift ()
  "Debug rift with RemedyBG."
  (interactive)
  (let ((default-directory "G:/repos/rift/"))
    (if (file-exists-p "C:/remedybg.exe")
        (async-shell-command "C:/remedybg.exe build\\Release\\rift.exe")
      (message "RemedyBG not found at C:/remedybg.exe"))))

(defun build-arc ()
  "Build arc Go project from cmd directory to build."
  (interactive)
  (let ((default-directory "G:/repos/arc/"))
    (compile "go build -v -o build/arc.exe ./cmd")))

(defun run-arc ()
  "Run arc game from build directory."
  (interactive)
  (let ((default-directory "G:/repos/arc/"))
    (if (file-exists-p "build/arc.exe")
        (async-shell-command "build\\arc.exe" "*Arc Output*")
      (async-shell-command "go run ./cmd" "*Arc Output*"))))

(defun debug-arc ()
  "Debug arc with delve from cmd directory."
  (interactive)
  (let ((default-directory "G:/repos/arc/cmd/"))
    (async-shell-command "dlv debug" "*Arc Debug*")))

(defun build-autonomica ()
  "Build autonomica C++ project with CMake."
  (interactive)
  (let ((default-directory "G:/repos/autonomica/"))
    (compile "cmake --build build --config Release")))

(defun run-autonomica ()
  "Run autonomica executable."
  (interactive)
  (let ((default-directory "G:/repos/autonomica/"))
    (async-shell-command "build\\Release\\autonomica.exe" "*Autonomica Output*")))

(defun debug-autonomica ()
  "Debug autonomica with RemedyBG."
  (interactive)
  (let ((default-directory "G:/repos/autonomica/"))
    (if (file-exists-p "C:/remedybg.exe")
        (async-shell-command "C:/remedybg.exe build\\Release\\autonomica.exe")
      (message "RemedyBG not found at C:/remedybg.exe"))))

(defun build-nan ()
  "Build nan Odin project."
  (interactive)
  (let ((default-directory "G:/repos/nan/src/"))
    (compile "odin build . -out:nan.exe")))

(defun build-nan-debug ()
  "Build nan Odin project with debug info for RemedyBG."
  (interactive)
  (let ((default-directory "G:/repos/nan/src/"))
    (compile "odin build . -out:nan.exe -debug")))

(defun run-nan ()
  "Run nan executable."
  (interactive)
  (let ((default-directory "G:/repos/nan/src/"))
    (async-shell-command "nan.exe" "*Nan Output*")))

(defun debug-nan ()
  "Debug nan with RemedyBG."
  (interactive)
  (let ((default-directory "G:/repos/nan/src/"))
    (if (file-exists-p "C:/remedybg.exe")
        (async-shell-command "C:/remedybg.exe nan.exe")
      (message "RemedyBG not found at C:/remedybg.exe"))))

(defun build-gridborn ()
  "Build gridborn Go project."
  (interactive)
  (let ((default-directory "G:/repos/gridborn/"))
    (compile "go build -v .")))

(defun run-gridborn ()
  "Run gridborn application."
  (interactive)
  (let ((default-directory "G:/repos/gridborn/"))
    (async-shell-command "go run ." "*Gridborn Output*")))

(defun debug-gridborn ()
  "Debug gridborn with delve."
  (interactive)
  (let ((default-directory "G:/repos/gridborn/"))
    (async-shell-command "dlv debug" "*Gridborn Debug*")))

(defun detect-current-project ()
  "Detect which project we're currently in based on file path."
  (let ((current-path (or (buffer-file-name) default-directory)))
    (cond
     ((string-match-p "rift" current-path) 'rift)
     ((string-match-p "arc" current-path) 'arc)
     ((string-match-p "autonomica" current-path) 'autonomica)
     ((string-match-p "nan" current-path) 'nan)
     ((string-match-p "gridborn" current-path) 'gridborn)
     (t nil))))

(defun smart-project-build ()
  "Build current project based on location."
  (interactive)
  (let ((project (detect-current-project)))
    (cond
     ((eq project 'rift) (build-rift))
     ((eq project 'arc) (build-arc))
     ((eq project 'autonomica) (build-autonomica))
     ((eq project 'nan) (build-nan))
     ((eq project 'gridborn) (build-gridborn))
     (t (message "Unknown project - not in rift, arc, autonomica, nan, or gridborn directory")))))

(defun smart-project-build-debug ()
  "Build current project in debug mode based on location."
  (interactive)
  (let ((project (detect-current-project)))
    (cond
     ((eq project 'rift) (build-rift-debug))
     ((eq project 'arc) (build-arc))
     ((eq project 'autonomica) (build-autonomica-debug))
     ((eq project 'nan) (build-nan-debug))
     ((eq project 'gridborn) (build-gridborn-debug))
     (t (message "Unknown project - not in rift, arc, autonomica, nan, or gridborn directory")))))

(defun smart-project-run ()
  "Run current project based on location."
  (interactive)
  (let ((project (detect-current-project)))
    (cond
     ((eq project 'rift) (run-rift))
     ((eq project 'arc) (run-arc))
     ((eq project 'autonomica) (run-autonomica))
     ((eq project 'nan) (run-nan))
     ((eq project 'gridborn) (run-gridborn))
     (t (message "Unknown project - not in rift, arc, autonomica, nan, or gridborn directory")))))

(defun smart-project-debug ()
  "Debug current project based on location."
  (interactive)
  (let ((project (detect-current-project)))
    (cond
     ((eq project 'rift) (debug-rift))
     ((eq project 'arc) (debug-arc))
     ((eq project 'autonomica) (debug-autonomica))
     ((eq project 'nan) (debug-nan))
     ((eq project 'gridborn) (debug-gridborn))
     (t (message "Unknown project - not in rift, arc, autonomica, nan, or gridborn directory")))))

;; Key bindings for smart project system
(global-set-key [f3] 'smart-project-build)        ; F3 = Build current project
(global-set-key [S-f3] 'smart-project-build-debug) ; Shift+F3 = Debug build current project
(global-set-key [f4] 'smart-project-run)          ; F4 = Run current project
(global-set-key [S-f4] 'smart-project-debug)      ; Shift+F4 = Debug current project

(message "Smart project build system configured for rift, arc, and autonomica")

(defun test-project-builds ()
  "Test project build system."
  (interactive)
  (let ((results '()))

    ;; Test project detection
    (push (format "Current project: %s" (detect-current-project)) results)

    ;; Test function definitions
    (dolist (func '(build-rift run-rift debug-rift
                    build-arc run-arc debug-arc
                    build-autonomica run-autonomica debug-autonomica))
      (if (fboundp func)
          (push (format "%s: DEFINED" func) results)
        (push (format "%s: MISSING" func) results)))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Project Builds Test*")
      (erase-buffer)
      (insert "=== Project Build System Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (display-buffer (current-buffer)))

    (message "Project builds test completed")))
