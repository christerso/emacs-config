;; Keybinding verification script
;; Load this with: M-x load-file RET verify-keybindings.el RET

(defun verify-keybindings ()
  "Test all configured keybindings to make sure they work."
  (interactive)
  (let ((results '())
        (tests '(
          ;; Essential keybindings
          ("C-x C-f" . find-file)
          ("C-x C-b" . switch-to-buffer)
          ("C-x b" . consult-buffer)
          ("C-x C-r" . recentf-open-files)
          ("C-x g" . magit-status)
          ("C-x C-c" . my-force-kill-emacs)

          ;; Window management
          ("C-x 2" . split-window-below)
          ("C-x 3" . split-window-right)
          ("C-x 0" . delete-window)
          ("C-x 1" . delete-other-windows)

          ;; Buffer navigation
          ("C-<tab>" . next-buffer)
          ("C-S-<tab>" . previous-buffer)

          ;; Build system
          ([f3] . smart-build)
          ([f4] . build-and-run)
          ([f5] . dream-build)
          ([f6] . dream-run)

          ;; Search
          ("C-c r" . my-ripgrep-search-current-dir)
          ("C-c R" . my-ripgrep-search-project-root)

          ;; Mode fixes
          ("C-c m" . force-mode-detection)

          ;; Dashboard
          ("C-c d" . open-dashboard)

          ;; Multiple cursors
          ("C-S-c C-S-c" . mc/edit-lines)
          ("C->" . mc/mark-next-like-this)
          ("C-<" . mc/mark-previous-like-this)

          ;; Quick copy
          ("C-<backspace>" . copy-entire-buffer)

          ;; Commenting
          ("C-/" . my-comment-dwim)
          )))

    (dolist (test tests)
      (let ((key (car test))
            (expected-func (cdr test))
            (actual-func (key-binding (kbd key))))
        (if (eq actual-func expected-func)
            (push (format "✓ %s → %s" key expected-func) results)
          (push (format "✗ %s → %s (expected %s)" key actual-func expected-func) results))))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Keybinding Verification*")
      (erase-buffer)
      (insert "=== KEYBINDING VERIFICATION RESULTS ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))

    (message "Keybinding verification complete - see *Keybinding Verification* buffer")))

;; Auto-run on load
(verify-keybindings)