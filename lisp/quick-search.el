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
(defun cs/find-files-dwim ()
  "Telescope `<leader>ff' for Emacs: fuzzy-find files by *name*, not by path.
In a project, fuzzy-match every tracked file (`project-find-file').  Outside a
project, fuzzy-match files under `default-directory' via fd (`consult-fd'),
falling back to the plain path prompt (`find-file') only if fd is unavailable.
Either way you type a fragment of the filename and pick from the vertico list —
you never walk a directory tree by hand."
  (interactive)
  (cond
   ((project-current) (call-interactively #'project-find-file))
   ((and (fboundp 'consult-fd) (executable-find "fd"))
    (call-interactively #'consult-fd))
   (t (call-interactively #'find-file))))

(defvar cs/search-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" #'cs/find-files-dwim)  ;; <leader>ff: fuzzy file by name
    (define-key map "p" #'project-find-file)   ;; file in project (explicit)
    (define-key map "F" #'find-file)           ;; plain path prompt (also C-x C-f)
    (define-key map "d" #'consult-fd)          ;; fast find anywhere (fd)
    (define-key map "r" #'consult-recent-file) ;; recent files
    (define-key map "g" #'consult-ripgrep)     ;; <leader>sg: live grep project
    (define-key map "l" #'consult-line)        ;; search lines in buffer
    (define-key map "b" #'consult-buffer)      ;; switch buffer (preview)
    (define-key map "B" #'ibuffer)             ;; full buffer manager
    (define-key map "j" #'consult-imenu)       ;; symbol in file
    (define-key map "i" #'consult-imenu-multi) ;; symbol across buffers
    (define-key map "s" #'consult-eglot-symbols) ;; <leader>sS: fuzzy LSP symbol in whole project
    (define-key map "k" #'cs/find-killed-buffer) ;; recently closed files
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

;; ---------------------------------------------------------------------------
;; Recently killed buffers — reopen a file you closed without leaving the fuzzy
;; buffer list.  Every killed file-visiting buffer is remembered (most recent
;; first, deduped, capped) and shown as an extra `consult-buffer' source under
;; a "Killed Buffer" heading; narrow to just those by typing "k SPC".  This is
;; the "show me buffers I also closed" view, in the same Telescope-style fuzzy
;; list as everything else (C-x b / C-c f b).
;; ---------------------------------------------------------------------------
(defcustom cs/killed-file-buffers-max 60
  "How many recently killed file buffers to remember for `consult-buffer'."
  :type 'integer :group 'convenience)

(defvar cs/killed-file-buffers nil
  "Absolute paths of recently killed file-visiting buffers, most recent first.")

(defun cs/remember-killed-buffer ()
  "Record the file of the buffer being killed, if it visits one."
  (when buffer-file-name
    (setq cs/killed-file-buffers
          (cons buffer-file-name (delete buffer-file-name cs/killed-file-buffers)))
    (when (> (length cs/killed-file-buffers) cs/killed-file-buffers-max)
      (setcdr (nthcdr (1- cs/killed-file-buffers-max) cs/killed-file-buffers) nil))))

(add-hook 'kill-buffer-hook #'cs/remember-killed-buffer)

(with-eval-after-load 'consult
  (defvar cs/consult-source-killed-buffer
    `( :name     "Killed Buffer"
       :narrow   ?k
       :category file
       :face     consult-file
       :history  file-name-history
       :state    ,#'consult--file-state    ;; preview + open on RET, like recent-file
       :items
       ,(lambda ()
          (seq-filter (lambda (f) (and (not (get-file-buffer f))
                                       (file-exists-p f)))
                      cs/killed-file-buffers)))
    "`consult-buffer' source listing recently killed file buffers.")
  ;; Show it in the normal buffer list (appended after the live buffers).
  (add-to-list 'consult-buffer-sources 'cs/consult-source-killed-buffer 'append))

(defun cs/find-killed-buffer ()
  "Fuzzy-pick from recently CLOSED files only (C-c f k).
The same list also appears inside `consult-buffer' under the Killed Buffer
heading; this jumps straight to it."
  (interactive)
  (consult-buffer '(cs/consult-source-killed-buffer)))

(provide 'quick-search)
;;; quick-search.el ends here
