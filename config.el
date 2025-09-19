;; Basic settings
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(global-display-line-numbers-mode 1)

;; No prompts
(setq confirm-kill-emacs nil)
(setq kill-buffer-query-functions nil)

;; Never ask about killing modified buffers
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))
(global-set-key (kbd "C-x k")
                (lambda ()
                  (interactive)
                  (kill-buffer (current-buffer))))

;; Dashboard
(setq initial-major-mode 'org-mode)
(setq initial-buffer-choice
      (lambda ()
        (let ((dashboard-file (expand-file-name "dashboard.org" user-emacs-directory)))
          (if (file-exists-p dashboard-file)
              (find-file-noselect dashboard-file)
            (get-buffer-create "*Dashboard*")))))

(defun open-dashboard ()
  "Open dashboard."
  (interactive)
  (find-file (expand-file-name "dashboard.org" user-emacs-directory)))

;; Essential keys
(global-set-key (kbd "C-c d") 'open-dashboard)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-auto-cleanup 'never)
(unless recentf-list
  (recentf-load-list))

;; LSP and completion will be handled by modules only

;; F3 key binding will be handled by project-builds module

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-check-signature nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Create modules directory if it doesn't exist
(let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
  (unless (file-directory-p modules-dir)
    (make-directory modules-dir t)))

;; Load ALL essential modules safely
(dolist (module '("core-packages"     ; Vertico, Consult, Company
                  "theme-system"       ; Dark theme and colors
                  "font-system"        ; Font enforcement
                  "language-support"   ; Odin, C, CMake, Go
                  "file-management"    ; Treemacs, Everything search
                  "window-navigation"  ; Window movement
                  "workspace-management" ; Project workspaces
                  "windows-unicode-fix" ; Unicode support
                  "auto-focus"         ; Auto focus behavior
                  "odin-unicode"       ; Odin-specific Unicode fixes
                  "project-builds"))   ; F3/F4 smart project build system
  (let ((module-file (expand-file-name (concat "modules/" module ".org") user-emacs-directory)))
    (when (file-exists-p module-file)
      (condition-case err
          (progn
            ;; Ensure we're in the right directory for org-babel
            (let ((default-directory user-emacs-directory))
              (org-babel-load-file module-file))
            (message "✓ LOADED: %s" module))
        (error
         (message "✗ FAILED: %s - %s" module (error-message-string err)))))))

(message "=== CONFIG LOADING COMPLETE ===")
