;;; llm-explain.el --- Explain the selected text with an LLM -*- lexical-binding: t; -*-
;;; Commentary:
;; Select any text — code, config, prose, an error message — and press C-c e:
;; an LLM explains what it is about in a side window on the right.
;;
;; The engine is `gptel' (https://github.com/karthink/gptel) talking to the
;; GitHub Copilot chat API — the same subscription copilot.el already uses, so
;; there is no extra API key and no extra cost.  gptel reuses the existing
;; ~/.config/github-copilot/apps.json login; if that ever expires it runs the
;; same one-time device-code browser flow as M-x copilot-login.
;;
;; This is the Emacs side of Neovim's `<leader>cce' (CopilotChatExplain).
;;
;; C-c e also works with no region: it then explains the function at point
;; (via `mark-defun') in code buffers, or errors with a hint otherwise.
;;; Code:

(eval-when-compile (require 'use-package))

(use-package gptel
  :defer t
  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-model 'gpt-4.1))   ;; included in every Copilot plan; swap freely

(defvar cs/llm-explain-buffer "*LLM Explain*")

(defconst cs/llm-explain-system
  "You are a precise senior engineer. Explain what the given text is about:
its purpose, how it works, and anything non-obvious or surprising. Be concise
— short paragraphs, no filler, no restating the code line by line. If the text
is not code, summarise what it says and what it is for."
  "System prompt for `cs/llm-explain'.")

(defun cs/llm-explain (beg end)
  "Explain what the selected text is about; result in a side window.
With no active region in a code buffer, explain the defun at point."
  (interactive
   (cond ((use-region-p) (list (region-beginning) (region-end)))
         ((derived-mode-p 'prog-mode)
          (save-excursion
            (mark-defun)
            (prog1 (list (region-beginning) (region-end))
              (deactivate-mark))))
         (t (user-error "Select the text to explain first"))))
  (require 'gptel)
  (let* ((lang (replace-regexp-in-string
                "\\(?:-ts\\)?-mode\\'" "" (symbol-name major-mode)))
         (text (buffer-substring-no-properties beg end))
         (nlines (count-lines beg end))         ;; in the SOURCE buffer
         (buf (get-buffer-create cs/llm-explain-buffer)))
    (with-current-buffer buf
      (when (and (fboundp 'markdown-mode) (not (derived-mode-p 'markdown-mode)))
        (markdown-mode))
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (insert (format "# Explaining %d lines of %s …\n" nlines lang))
      (visual-line-mode 1))
    (display-buffer buf '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.42)))
    (gptel-request
        (format "Explain what this %s is about:\n\n```%s\n%s\n```" lang lang text)
      :system cs/llm-explain-system
      :buffer buf
      :callback
      (lambda (response info)
        (let ((b (plist-get info :buffer)))
          (when (buffer-live-p b)
            (with-current-buffer b
              (erase-buffer)
              (insert (or (and (stringp response) response)
                          (format "LLM request failed: %s"
                                  (plist-get info :status))))
              (goto-char (point-min)))))))))

(global-set-key (kbd "C-c e") #'cs/llm-explain)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c e" "explain (LLM)"))

(provide 'llm-explain)
;;; llm-explain.el ends here
