;;; run-console.el --- Run the current program in a persistent external console -*- lexical-binding: t; -*-
;;; Commentary:
;; `C-c m r' (cs/console-run) launches the current project's program in a REAL
;; external terminal window that STAYS OPEN after the program exits: it prints
;; the exit code and waits for Enter, so crashes and stdout/stderr stay
;; readable instead of the window vanishing.
;;
;; The run command is guessed from the project (Makefile `run' target, go.mod,
;; build.zig, *.odin, current major mode, …) and offered for editing — accept
;; with RET, or type anything else.  Override the terminal emulator with e.g.
;;   (setq cs/console-terminal "konsole")
;;; Code:

(require 'project)
(require 'seq)

(defgroup cs/console nil
  "Run programs in a persistent external console window."
  :group 'tools)

(defcustom cs/console-terminal nil
  "Terminal emulator to launch, or nil to auto-detect.
A string such as \"konsole\", \"kitty\", \"ghostty\", or \"alacritty\"."
  :type '(choice (const :tag "Auto-detect" nil) (string :tag "Command"))
  :group 'cs/console)

(defvar cs/console--terminal-priority
  '("ghostty" "kitty" "alacritty" "konsole" "wezterm" "foot"
    "gnome-terminal" "xfce4-terminal" "xterm")
  "Order in which to auto-detect a terminal emulator.")

(defun cs/console--terminal ()
  "Return the terminal command to use, or signal a `user-error'."
  (or (and cs/console-terminal
           (executable-find cs/console-terminal)
           cs/console-terminal)
      (seq-find #'executable-find cs/console--terminal-priority)
      (user-error "No terminal emulator found; set `cs/console-terminal'")))

(defun cs/console--argv (term inner)
  "Build the argv list that runs shell snippet INNER inside terminal TERM."
  (pcase (file-name-nondirectory term)
    ("gnome-terminal" (list term "--" "sh" "-c" inner))
    ("wezterm"        (list term "start" "--" "sh" "-c" inner))
    ("xfce4-terminal" (list term "-x" "sh" "-c" inner))
    ((or "kitty" "foot") (list term "sh" "-c" inner))
    ;; ghostty, alacritty, konsole, xterm and most others take -e.
    (_ (list term "-e" "sh" "-c" inner))))

(defun cs/console--run-dir ()
  "Directory to run in: the project root if any, else `default-directory'."
  (if-let ((proj (project-current nil)))
      (project-root proj)
    default-directory))

(defun cs/console--make-has-run-p (dir)
  "Non-nil when the Makefile in DIR defines a `run' target."
  (let ((mf (expand-file-name "Makefile" dir)))
    (and (file-readable-p mf)
         (with-temp-buffer
           (insert-file-contents mf)
           (goto-char (point-min))
           (re-search-forward "^run[ \t]*:" nil t)))))

(defun cs/console--default-command ()
  "Best-guess run command for the current buffer/project."
  (let ((dir (cs/console--run-dir)))
    (cond
     ((and (file-exists-p (expand-file-name "Makefile" dir))
           (cs/console--make-has-run-p dir))      "make run")
     ((file-exists-p (expand-file-name "build.zig" dir)) "zig build run")
     ((file-exists-p (expand-file-name "go.mod" dir))    "go run .")
     ((derived-mode-p 'go-mode 'go-ts-mode)              "go run .")
     ((and (derived-mode-p 'zig-mode) buffer-file-name)
      (concat "zig run " (shell-quote-argument
                          (file-name-nondirectory buffer-file-name))))
     ((derived-mode-p 'odin-mode)
      (if (file-directory-p (expand-file-name "src" dir)) "odin run src" "odin run ."))
     ((file-exists-p (expand-file-name "Makefile" dir))  "make run")
     (t ""))))

(defun cs/console-run (command)
  "Run COMMAND in an external terminal that stays open after it exits.
Interactively, offers a project-aware default; edit then RET to confirm."
  (interactive
   (list (read-shell-command "Run in console: " (cs/console--default-command))))
  (when (string-empty-p (string-trim command))
    (user-error "No command to run"))
  (let* ((term (cs/console--terminal))
         (default-directory (cs/console--run-dir))
         ;; Run the command, then hold the window: show the exit code and wait
         ;; for Enter so the console never closes immediately.
         (hold (concat command
                       "; status=$?; "
                       "printf '\\n\\033[1;33m[process exited %s] — press Enter to close\\033[0m\\n' \"$status\"; "
                       "read _"))
         (argv (cs/console--argv term hold)))
    (message "Console: %s  (%s, in %s)"
             command (file-name-nondirectory term) default-directory)
    (apply #'start-process "cs-console" nil argv)))

(global-set-key (kbd "C-c m r") #'cs/console-run)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c m" "make/run"))

(provide 'run-console)
;;; run-console.el ends here
