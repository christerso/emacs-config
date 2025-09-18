;; Install perspective.el for project workspaces
(unless (package-installed-p 'perspective)
  (package-install 'perspective))

(require 'perspective)

;; Configure perspective mode with proper prefix key
(setq persp-mode-prefix-key (kbd "C-c M-p"))
(setq persp-suppress-no-prefix-key-warning t)

(persp-mode)

;; Project workspace variables
(defvar my-project-workspaces '()
  "List of active project workspaces with metadata.")

(defvar my-current-project-info nil
  "Current project information for mode line display.")

;; Project color assignment for visual distinction
(defun my-assign-project-color (project-name)
  "Assign a unique color to a project based on its name."
  (let ((colors '("#569cd6" "#4ec9b0" "#ce9178" "#c586c0" "#dcdcaa"
                  "#b5cea8" "#9cdcfe" "#f44747" "#ff6b6b" "#4ecdc4")))
    (nth (mod (string-hash project-name) (length colors)) colors)))

;; Create project workspace
(defun my-create-project-workspace (project-path &optional workspace-name)
  "Create a new workspace for a project with visual identification."
  (interactive (list (read-directory-name "Project directory: ")
                     (read-string "Workspace name (optional): ")))
  (let* ((project-name (or (and (not (string-empty-p workspace-name)) workspace-name)
                          (file-name-nondirectory (directory-file-name project-path))))
         (workspace-id (format "%s" project-name))
         (project-color (my-assign-project-color project-name)))

    ;; Create or switch to perspective
    (persp-switch workspace-id)

    ;; Store project metadata
    (setq my-current-project-info
          `(:name ,project-name
            :path ,project-path
            :color ,project-color
            :workspace ,workspace-id))

    ;; Add to workspace list if not already there
    (unless (assoc workspace-id my-project-workspaces)
      (push (cons workspace-id my-current-project-info) my-project-workspaces))

    ;; Open project files
    (dired project-path)

    (message "Created workspace '%s' for project '%s'" workspace-id project-name)))

;; Switch between workspaces
(defun my-switch-project-workspace ()
  "Switch between active project workspaces with preview."
  (interactive)
  (let* ((workspace-choices (mapcar (lambda (ws)
                                     (let ((info (cdr ws)))
                                       (format "%s (%s)"
                                              (car ws)
                                              (plist-get info :path))))
                                   my-project-workspaces))
         (choice (completing-read "Switch to workspace: " workspace-choices)))
    (when choice
      (let* ((workspace-name (car (split-string choice " (")))
             (workspace-info (cdr (assoc workspace-name my-project-workspaces))))
        (persp-switch workspace-name)
        (setq my-current-project-info workspace-info)
        (message "Switched to workspace: %s" workspace-name)))))

;; List workspaces
(defun my-list-project-workspaces ()
  "Show all active project workspaces in a buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*Project Workspaces*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Active Project Workspaces ===\n\n")
      (if my-project-workspaces
          (dolist (ws my-project-workspaces)
            (let* ((workspace-name (car ws))
                   (info (cdr ws))
                   (project-name (plist-get info :name))
                   (project-path (plist-get info :path))
                   (project-color (plist-get info :color)))
              (insert (format "[%s] %s\n    Path: %s\n    Color: %s\n\n"
                             workspace-name project-name project-path project-color))))
        (insert "No active project workspaces.\n"))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)))

;; Key bindings for project workspace management
(global-set-key (kbd "C-c w c") 'my-create-project-workspace)
(global-set-key (kbd "C-c w s") 'my-switch-project-workspace)
(global-set-key (kbd "C-c w l") 'my-list-project-workspaces)

(defun test-workspace-management-module ()
  "Test workspace management module."
  (interactive)
  (let ((results '()))

    ;; Test function definitions
    (dolist (func '(my-create-project-workspace my-switch-project-workspace my-list-project-workspaces))
      (if (fboundp func)
          (push (format "%s: DEFINED" func) results)
        (push (format "%s: MISSING" func) results)))

    ;; Test perspective
    (if (featurep 'perspective)
        (push "perspective: LOADED" results)
      (push "perspective: NOT LOADED" results))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Workspace Test*")
      (erase-buffer)
      (insert "=== Workspace Management Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (display-buffer (current-buffer)))

    (message "Workspace management test completed")))
