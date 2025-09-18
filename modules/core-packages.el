;; Projectile for project management
(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(require 'projectile)
(projectile-mode +1)

;; Vertico ecosystem - modern completion
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

(unless (package-installed-p 'consult)
  (package-install 'consult))

(unless (package-installed-p 'marginalia)
  (package-install 'marginalia))

(unless (package-installed-p 'orderless)
  (package-install 'orderless))

;; Load and configure
(require 'vertico)
(require 'consult)
(require 'marginalia)
(require 'orderless)

(vertico-mode 1)
(marginalia-mode 1)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; Enhanced keybindings
(global-set-key (kbd "M-x") 'consult-M-x)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-s") 'consult-line)

;; Eglot for LSP
(require 'eglot)
(add-to-list 'eglot-server-programs '(c-mode . ("clangd")))
(add-hook 'c-mode-hook 'eglot-ensure)

(defun test-core-packages-module ()
  "Test core packages module functionality."
  (interactive)
  (let ((results '()))

    ;; Test package availability
    (dolist (pkg '(projectile vertico consult marginalia orderless))
      (if (featurep pkg)
          (push (format "%s: LOADED" pkg) results)
        (push (format "%s: NOT LOADED" pkg) results)))

    ;; Test key bindings
    (dolist (key '("M-x" "C-x b" "C-s"))
      (let ((binding (key-binding (kbd key))))
        (push (format "%s -> %s" key binding) results)))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Core Packages Test*")
      (erase-buffer)
      (insert "=== Core Packages Module Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (display-buffer (current-buffer)))

    (message "Core packages test completed")))
