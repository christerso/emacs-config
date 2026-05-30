;;; quick-search.el --- Telescope-style fuzzy search everything -*- lexical-binding: t; -*-
;;; Commentary:
;; The Emacs equivalent of LazyVim's Telescope (<leader>ff / <leader>sg).
;; The engine already lives in config.org: vertico (vertical list) + orderless
;; (fuzzy, space-separated matching) + marginalia (annotations) + consult (rich
;; sources with live preview).  This module makes it *look* the part —
;; minibuffer icons via `nerd-icons-completion' — and binds a single coherent
;; "find / search everything" group under the C-c f prefix.
;;
;; All search/find keybindings live HERE (relocated out of config.org) so there
;; is exactly one source of truth for them.
;;; Code:

(eval-when-compile (require 'use-package))

;; Gorgeous: file-type / command icons in the minibuffer candidate list.
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; A little more breathing room + dynamic resize for the candidate list.
(with-eval-after-load 'vertico
  (setq vertico-count 15
        vertico-resize t))

;; ---------------------------------------------------------------------------
;; The "find / search everything" keymap — C-c f <key>
;; Mnemonics mirror LazyVim: f=files, g=grep, b=buffers, r=recent, l=lines …
;; ---------------------------------------------------------------------------
(global-set-key (kbd "C-c f f") #'find-file)             ;; file by path
(global-set-key (kbd "C-c f p") #'project-find-file)     ;; <leader>ff: file in project
(global-set-key (kbd "C-c f d") #'consult-fd)            ;; fast find anywhere (fd)
(global-set-key (kbd "C-c f r") #'consult-recent-file)   ;; recent files
(global-set-key (kbd "C-c f g") #'consult-ripgrep)       ;; <leader>sg: live grep project
(global-set-key (kbd "C-c f l") #'consult-line)          ;; search lines in buffer
(global-set-key (kbd "C-c f b") #'consult-buffer)        ;; switch buffer (preview)
(global-set-key (kbd "C-c f j") #'consult-imenu)         ;; symbol in file
(global-set-key (kbd "C-c f i") #'consult-imenu-multi)   ;; symbol across buffers
(global-set-key (kbd "C-c f o") #'consult-outline)       ;; outline / headings
(global-set-key (kbd "C-c f m") #'consult-mark)          ;; jump to a mark
(global-set-key (kbd "C-c f e") #'consult-flymake)       ;; diagnostics
(global-set-key (kbd "C-c f G") #'consult-git-grep)      ;; grep tracked files

;; Make the everyday buffer switch and yank-pop use consult's previewed UI too.
(global-set-key (kbd "C-x b") #'consult-buffer)
(with-eval-after-load 'consult
  (global-set-key (kbd "M-y") #'consult-yank-pop))

(provide 'quick-search)
;;; quick-search.el ends here
