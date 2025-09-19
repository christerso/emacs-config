;; Download and install voidlight theme
(let ((theme-dir (expand-file-name "themes" user-emacs-directory))
      (theme-file (expand-file-name "themes/voidlight-theme.el" user-emacs-directory)))

  ;; Create themes directory if it doesn't exist
  (unless (file-directory-p theme-dir)
    (make-directory theme-dir t))

  ;; Download theme file if it doesn't exist
  (unless (file-exists-p theme-file)
    (url-copy-file "https://raw.githubusercontent.com/christerso/emacs-voidlight-theme/main/voidlight-theme.el"
                   theme-file t))

  ;; Add themes directory to custom theme load path
  (add-to-list 'custom-theme-load-path theme-dir)

  ;; Load the voidlight theme
  (load-theme 'voidlight t)

  ;; Darker selection color for better visibility
  (set-face-background 'region "#1e2124")

  ;; Make dark blue text much brighter and more readable
  (set-face-foreground 'font-lock-function-name-face "#7db3ff")  ; Bright blue
  (set-face-foreground 'font-lock-type-face "#7db3ff")           ; Bright blue for types
  (set-face-foreground 'font-lock-builtin-face "#7db3ff")        ; Bright blue for builtins
  )

;; Additional muted color adjustments
(defun apply-muted-colors ()
  "Apply muted color scheme for comfortable coding."
  (interactive)
  ;; Override any overly bright colors with muted versions
  (set-face-foreground 'font-lock-keyword-face "#7aa2f7")      ; Muted blue
  (set-face-foreground 'font-lock-string-face "#9ece6a")       ; Muted green
  (set-face-foreground 'font-lock-comment-face "#565f89")      ; Muted gray
  (set-face-foreground 'font-lock-function-name-face "#bb9af7") ; Muted purple
  (set-face-foreground 'font-lock-variable-name-face "#c0caf5") ; Muted white
  (set-face-foreground 'font-lock-type-face "#2ac3de")         ; Muted cyan
  (set-face-foreground 'font-lock-constant-face "#ff9e64")     ; Muted orange

  (message "Muted color scheme applied"))

;; Apply after theme loads (DISABLED to preserve voidlight theme colors)
;; (add-hook 'after-init-hook 'apply-muted-colors)
(global-set-key (kbd "C-c t m") 'apply-muted-colors)

(defun test-theme-system-module ()
  "Test theme system module."
  (interactive)
  (let ((results '()))

    ;; Test theme availability
    (if (custom-theme-enabled-p 'voidlight)
        (push "voidlight theme: ACTIVE" results)
      (push "voidlight theme: NOT ACTIVE" results))

    ;; Test function definitions
    (if (fboundp 'apply-muted-colors)
        (push "apply-muted-colors: DEFINED" results)
      (push "apply-muted-colors: MISSING" results))

    ;; Test current colors
    (let ((keyword-color (face-foreground 'font-lock-keyword-face)))
      (push (format "Keyword color: %s" keyword-color) results))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Theme Test*")
      (erase-buffer)
      (insert "=== Theme System Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (display-buffer (current-buffer)))

    (message "Theme system test completed")))