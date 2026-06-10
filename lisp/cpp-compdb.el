;;; cpp-compdb.el --- Zero-interaction compile_commands.json for clangd -*- lexical-binding: t; -*-
;;; Commentary:
;; clangd's full symbol index — project-wide references, jump into library and
;; system headers, completion across translation units — only lights up when it
;; finds a compile_commands.json.  This module makes that automatic:
;;
;;   1. Opening any C/C++ file: if the project root already has
;;      compile_commands.json, do nothing.
;;   2. If a build directory (build/, build-*/, out/, cmake-build-*/) has one,
;;      symlink it into the root silently.
;;   3. If the project is CMake-based and has no database anywhere, run
;;      `cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON' asynchronously in
;;      the background, then symlink the result and nudge eglot to reconnect.
;;
;; Everything is quiet (messages only, never prompts) and never runs twice for
;; the same root in one session.  Non-CMake projects without a database are
;; left alone — clangd still works single-file via its fallback flags, and
;; Makefile projects can generate one with `bear -- make' or `make compdb'.
;;; Code:

(defvar cs/compdb--attempted (make-hash-table :test 'equal)
  "Project roots already handled this session (avoid repeat cmake runs).")

(defconst cs/compdb--build-dirs
  '("build" "build-debug" "build-release" "out" "cmake-build-debug" "cmake-build-release")
  "Build directories searched for an existing compile_commands.json.")

(defun cs/compdb--root ()
  "Locate the project root for the current buffer (CMakeLists.txt > project.el)."
  (or (locate-dominating-file default-directory "CMakeLists.txt")
      (when-let ((proj (project-current)))
        (project-root proj))))

(defun cs/compdb--existing-db (root)
  "Return the path of a compile_commands.json under ROOT's build dirs, if any."
  (seq-some (lambda (dir)
              (let ((db (expand-file-name (concat dir "/compile_commands.json") root)))
                (and (file-exists-p db) db)))
            cs/compdb--build-dirs))

(defun cs/compdb--link (db root)
  "Symlink DB as ROOT/compile_commands.json."
  (let ((target (expand-file-name "compile_commands.json" root)))
    (unless (file-exists-p target)
      (make-symbolic-link db target t)
      (message "clangd: linked %s -> %s" target db))))

(defun cs/compdb--reconnect-eglot ()
  "Restart eglot in C/C++ buffers so clangd picks up the new database."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
                 (bound-and-true-p eglot--managed-mode))
        (ignore-errors (eglot-reconnect (eglot-current-server)))))))

(defun cs/compdb--generate (root)
  "Run cmake in the background to produce compile_commands.json for ROOT."
  (let ((default-directory root))
    (message "clangd: generating compile_commands.json for %s ..." root)
    (make-process
     :name "cs-compdb-cmake"
     :buffer " *cs-compdb-cmake*"
     :command '("cmake" "-S" "." "-B" "build" "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((db (expand-file-name "build/compile_commands.json" root)))
           (if (and (zerop (process-exit-status proc)) (file-exists-p db))
               (progn
                 (cs/compdb--link db root)
                 (cs/compdb--reconnect-eglot)
                 (message "clangd: compile_commands.json ready for %s" root))
             (message "clangd: cmake configure failed for %s (see %s)"
                      root (buffer-name (process-buffer proc))))))))))

(defun cs/compdb-ensure ()
  "Make sure clangd has a compilation database for this buffer's project."
  (when buffer-file-name
    (when-let ((root (cs/compdb--root)))
      (unless (or (gethash root cs/compdb--attempted)
                  (file-exists-p (expand-file-name "compile_commands.json" root)))
        (puthash root t cs/compdb--attempted)
        (if-let ((db (cs/compdb--existing-db root)))
            (cs/compdb--link db root)
          (when (and (file-exists-p (expand-file-name "CMakeLists.txt" root))
                     (executable-find "cmake"))
            (cs/compdb--generate root)))))))

(dolist (hook '(c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook))
  (add-hook hook #'cs/compdb-ensure))

(provide 'cpp-compdb)
;;; cpp-compdb.el ends here
