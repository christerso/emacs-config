;;; bottom-terminal.el --- A toggleable terminal docked at the bottom -*- lexical-binding: t; -*-
;;; Commentary:
;; A small terminal in a window across the bottom of the frame, toggled with a
;; single key.  Hiding it just removes the window — the shell process keeps
;; running in the background, so you can flip it in and out without losing state.
;;
;; Backend is `eat' (https://codeberg.org/akib/emacs-eat) — pure elisp, no
;; compile step, works in GUI and TTY, handles colours and full-screen TUIs.
;; `run-console.el' launches an EXTERNAL terminal for running a built program;
;; this is the in-Emacs scratch terminal for quick commands.
;;
;; Keys:
;;   C-c m t   toggle (show / hide) the bottom terminal
;;   C-`       same — GUI muscle memory (also hides from inside the terminal)
;;; Code:

(eval-when-compile (require 'use-package))

(defvar cs/terminal-buffer-name "*bottom-terminal*"
  "Buffer name of the dedicated bottom terminal.")

(defcustom cs/terminal-height 0.30
  "Fraction of the frame height used by the bottom terminal window."
  :type 'number :group 'convenience)

(use-package eat
  :commands (eat eat-mode)
  :custom
  (eat-kill-buffer-on-exit t)          ;; type `exit' and the buffer goes away
  :config
  ;; Always show the bottom terminal as a dedicated bottom side window.
  (add-to-list 'display-buffer-alist
               `(,(regexp-quote cs/terminal-buffer-name)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . ,cs/terminal-height)
                 (dedicated . t)))
  ;; Let C-` hide the terminal from *inside* it. C-c is deliberately left alone
  ;; so Ctrl-C still interrupts programs running in the shell.
  (with-eval-after-load 'eat
    (when (boundp 'eat-semi-char-mode-map)
      (define-key eat-semi-char-mode-map (kbd "C-`") #'cs/toggle-terminal))))

(defun cs/toggle-terminal ()
  "Toggle a small `eat' terminal docked at the bottom of the frame.
Open it on first use; hide it (shell keeps running) when its window is
showing; re-show the same buffer when it exists but is hidden."
  (interactive)
  (let* ((buf (get-buffer cs/terminal-buffer-name))
         (win (and buf (get-buffer-window buf))))
    (cond
     (win (delete-window win))                       ;; visible -> background it
     (buf (select-window (display-buffer buf)))      ;; hidden  -> bring it back
     (t   (let ((eat-buffer-name cs/terminal-buffer-name))
            (save-window-excursion (eat))            ;; create without stealing focus
            (select-window (display-buffer
                            (get-buffer cs/terminal-buffer-name))))))))

(global-set-key (kbd "C-c m t") #'cs/toggle-terminal)
(global-set-key (kbd "C-`")     #'cs/toggle-terminal)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c m t" "bottom-terminal"))

(provide 'bottom-terminal)
;;; bottom-terminal.el ends here
