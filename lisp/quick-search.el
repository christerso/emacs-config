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
;; The "find / search everything" prefix map.
;; Mnemonics mirror LazyVim: f=files, g=grep, b=buffers, r=recent, l=lines …
;; ---------------------------------------------------------------------------
(defvar cs/search-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'find-file)           ;; file by path
    (define-key map "p" #'project-find-file)   ;; <leader>ff: file in project
    (define-key map "d" #'consult-fd)          ;; fast find anywhere (fd)
    (define-key map "r" #'consult-recent-file) ;; recent files
    (define-key map "g" #'consult-ripgrep)     ;; <leader>sg: live grep project
    (define-key map "l" #'consult-line)        ;; search lines in buffer
    (define-key map "b" #'consult-buffer)      ;; switch buffer (preview)
    (define-key map "B" #'ibuffer)             ;; full buffer manager
    (define-key map "j" #'consult-imenu)       ;; symbol in file
    (define-key map "i" #'consult-imenu-multi) ;; symbol across buffers
    (define-key map "o" #'consult-outline)     ;; outline / headings
    (define-key map "m" #'consult-mark)        ;; jump to a mark
    (define-key map "e" #'consult-flymake)     ;; diagnostics
    (define-key map "G" #'consult-git-grep)    ;; grep tracked files
    map)
  "Telescope-style find/search prefix keymap.")

;; Bound to BOTH C-c f and C-x f — the latter replaces the default
;; `set-fill-column' (easy to fat-finger when reaching for find), so the two
;; prefixes are now identical fuzzy-search entry points.
(global-set-key (kbd "C-c f") cs/search-map)
(global-set-key (kbd "C-x f") cs/search-map)

;; Make the everyday buffer commands use consult's fuzzy, previewed menu.
;; Both C-x b and C-x C-b open the fuzzy-find buffer list; the plain ibuffer
;; manager stays one key away on  C-c f B / C-x f B.
(global-set-key (kbd "C-x b")   #'consult-buffer)
(global-set-key (kbd "C-x C-b") #'consult-buffer)
(with-eval-after-load 'consult
  (global-set-key (kbd "M-y") #'consult-yank-pop))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c f" "find/search"
    "C-x f" "find/search"))

(provide 'quick-search)
;;; quick-search.el ends here
