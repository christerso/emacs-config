(defvar my-protected-face-patterns
  '("icon" "treemacs" "all-the-icons" "nerd-icons" "dired-directory"
    "magit-diff" "git-gutter" "flycheck" "company-tooltip" "lsp-ui"
    "rainbow-delimiters" "hl-line" "region" "isearch" "lazy-highlight"
    "show-paren" "cursor" "fringe" "vertical-border")
  "Face name patterns that should not be modified by font enforcement.")

(defun my-face-should-be-protected-p (face-name)
  "Return t if FACE-NAME should be protected from font changes."
  (let ((face-str (symbol-name face-name)))
    (cl-some (lambda (pattern)
               (string-match-p pattern face-str))
             my-protected-face-patterns)))

(defun force-jetbrains-font-selectively ()
  "Enforce JetBrains Mono font only on text faces, preserving special symbols."
  (when (member "JetBrainsMono Nerd Font" (font-family-list))
    ;; Core text faces
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 110)
    (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 110)
    (set-face-attribute 'variable-pitch nil :family "JetBrainsMono Nerd Font" :height 110)

    ;; Programming language faces
    (dolist (face '(font-lock-comment-face font-lock-string-face font-lock-keyword-face
                    font-lock-function-name-face font-lock-variable-name-face
                    font-lock-type-face font-lock-constant-face font-lock-builtin-face))
      (when (facep face)
        (set-face-attribute face nil :family "JetBrainsMono Nerd Font")))

    (message "JetBrains Mono font applied selectively")))

;; Apply font enforcement aggressively
(force-jetbrains-font-selectively)

;; Nuclear font enforcement - run immediately and periodically
(add-hook 'after-init-hook 'force-jetbrains-font-selectively)
(add-hook 'window-setup-hook 'force-jetbrains-font-selectively)
(add-hook 'after-make-frame-functions
          (lambda (frame) (force-jetbrains-font-selectively)))

;; Force fonts every 3 seconds until they stick
(run-with-idle-timer 3 t 'force-jetbrains-font-selectively)

;; Manual font fix command
(defun nuclear-font-fix ()
  "Aggressively force JetBrains font everywhere."
  (interactive)
  (when (member "JetBrainsMono Nerd Font" (font-family-list))
    ;; Set EVERY possible face
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 110)
    (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 110)
    (set-face-attribute 'variable-pitch nil :family "JetBrainsMono Nerd Font" :height 110)
    (set-face-attribute 'mode-line nil :family "JetBrainsMono Nerd Font" :height 100)
    (set-face-attribute 'mode-line-inactive nil :family "JetBrainsMono Nerd Font" :height 100)

    ;; Force buffer-specific font
    (setq buffer-face-mode-face '(:family "JetBrainsMono Nerd Font" :height 110))
    (buffer-face-mode 1)

    ;; Force redisplay
    (redraw-display)
    (message "NUCLEAR FONT FIX APPLIED - JetBrains font forced everywhere")))

(global-set-key (kbd "C-c M-f") 'nuclear-font-fix)

;; Comprehensive Unicode setup
(when (member "JetBrainsMono Nerd Font" (font-family-list))
  ;; UTF-8 everywhere
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; Font configuration for Unicode
  (set-fontset-font t 'unicode "JetBrainsMono Nerd Font" nil 'prepend)
  (set-fontset-font t 'unicode "Segoe UI Symbol" nil 'append)
  (set-fontset-font t 'unicode "Segoe UI Emoji" nil 'append)

  ;; Critical symbols
  (set-fontset-font t ?\u2192 "JetBrainsMono Nerd Font" nil 'prepend) ; →
  (set-fontset-font t ?\u2190 "JetBrainsMono Nerd Font" nil 'prepend) ; ←
  (set-fontset-font t ?\u2022 "JetBrainsMono Nerd Font" nil 'prepend) ; •

  (message "Unicode font system configured"))

(defun test-font-system-module ()
  "Test font system module."
  (interactive)
  (let ((results '()))

    ;; Test font availability
    (if (member "JetBrainsMono Nerd Font" (font-family-list))
        (push "JetBrains font: AVAILABLE" results)
      (push "JetBrains font: MISSING" results))

    ;; Test function definitions
    (dolist (func '(force-jetbrains-font-selectively my-face-should-be-protected-p))
      (if (fboundp func)
          (push (format "%s: DEFINED" func) results)
        (push (format "%s: MISSING" func) results)))

    ;; Test current font
    (let ((current-font (face-attribute 'default :family)))
      (push (format "Current font: %s" current-font) results))

    ;; Display results
    (with-current-buffer (get-buffer-create "*Font System Test*")
      (erase-buffer)
      (insert "=== Font System Test ===\n\n")
      (dolist (result (reverse results))
        (insert (format "%s\n" result)))
      (display-buffer (current-buffer)))

    (message "Font system test completed")))
