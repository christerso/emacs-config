;; Install treemacs packages
(unless (package-installed-p 'treemacs)
  (package-install 'treemacs))

(unless (package-installed-p 'treemacs-projectile)
  (package-install 'treemacs-projectile))

(require 'treemacs)
(require 'treemacs-projectile)

;; Basic treemacs configuration
(setq treemacs-width 35
      treemacs-follow-after-init t
      treemacs-show-hidden-files t)

;; Key bindings
(global-set-key (kbd "M-0") 'treemacs-select-window)
(global-set-key (kbd "C-x t t") 'treemacs)
(global-set-key (kbd "C-x t C-t") 'treemacs-find-file)

;; Auto-follow and sync
(treemacs-follow-mode t)
(treemacs-filewatch-mode t)

;; Treemacs project management
(defun treemacs-nuke-and-rebuild ()
  "Nuclear option: completely reset Treemacs and add projects."
  (interactive)
  (message "NUKING Treemacs workspace...")

  ;; Kill all Treemacs buffers
  (dolist (buffer (buffer-list))
    (when (string-match-p "Treemacs" (buffer-name buffer))
      (kill-buffer buffer)))

  ;; Clear all workspace data
  (setq treemacs--workspaces (make-hash-table :test 'equal))

  ;; Add projects fresh
  (let ((projects '(("arc" . "G:/repos/arc/")
                    ("gridborn" . "G:/repos/gridborn/")
                    ("rift" . "G:/repos/rift/")
                    ("nan" . "G:/repos/nan/"))))

    (message "Adding fresh projects...")

    ;; Add each project
    (dolist (project projects)
      (let ((name (car project))
            (path (cdr project)))
        (when (file-directory-p path)
          (condition-case err
              (progn
                (treemacs-add-project-to-workspace path name)
                (message "✓ Added project: %s" name))
            (error
             (message "✗ Failed to add %s: %s" name (error-message-string err)))))))

    (treemacs)
    (message "Treemacs NUKED and rebuilt successfully!")))

;; Simple manual project add
(defun add-treemacs-project (name path)
  "Add a single project to Treemacs."
  (interactive (list (read-string "Project name: ")
                     (read-directory-name "Project path: ")))
  (condition-case err
      (progn
        (treemacs-add-project-to-workspace path name)
        (message "✓ Added project: %s -> %s" name path))
    (error
     (message "✗ Failed to add project: %s" (error-message-string err)))))

;; Commands
(global-set-key (kbd "C-c t n") 'treemacs-nuke-and-rebuild)  ; Nuclear option
(global-set-key (kbd "C-c t a") 'add-treemacs-project)       ; Add single project

(when (eq system-type 'windows-nt)
  ;; Simple everything search
  (defun everything-search ()
    "Search files using Everything."
    (interactive)
    (let* ((query (read-string "Everything search: "))
           (results (when (> (length query) 0)
                      (split-string
                       (shell-command-to-string (format "C:\\Users\\chris\\es.exe -n 20 \"%s\"" query))
                       "\n" t))))
      (if results
          (let ((choice (completing-read "Select file: " results)))
            (when choice
              (if (file-exists-p choice)
                  (find-file choice)
                (message "File not found: %s" choice))))
        (message "No results found for: %s" query))))

  (global-set-key (kbd "C-c s") 'everything-search))

(defun test-file-management-module ()
  "Test file management module."
  (interactive)
  (let ((results '()))

    ;; Test treemacs
    (if (featurep 'treemacs)
        (push "treemacs: LOADED" results)
      (push "treemacs: NOT LOADED" results))

    ;; Test everything search (Windows only)
    (when (eq system-type 'windows-nt)
      (if (fboundp 'everything-search)
          (push "everything-search: DEFINED" results)
        (push "everything-search: MISSING" results)))

    ;; Display results
    (with-current-buffer (get-buffer-create "*File Management Test*")
      (erase-buffer)
      (insert "=== File Management Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (display-buffer (current-buffer)))

    (message "File management test completed")))
