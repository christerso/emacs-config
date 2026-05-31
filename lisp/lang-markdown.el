;;; lang-markdown.el --- Markdown editing & highlighting -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs ships no markdown major mode, so this uses `markdown-mode'
;; (https://github.com/jrblevin/markdown-mode), the standard one.
;;
;; `.md' opens in `gfm-mode' (GitHub-Flavored Markdown — task lists,
;; strikethrough, fenced code), since markdown here is mostly README / repo
;; docs; `.markdown' uses plain `markdown-mode'.  Fenced code blocks are
;; fontified in their own language's major mode.
;;
;; Highlighting needs no external tools.  `markdown-command' (pandoc, present on
;; this box) is only used for the optional `C-c C-c v' HTML preview/export.
;;; Code:

(eval-when-compile (require 'use-package))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"        . gfm-mode)
         ("\\.markdown\\'"  . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t) ;; highlight fenced code in its lang
  (markdown-header-scaling t)               ;; size headings by level
  (markdown-enable-math t)                  ;; $...$ / $$...$$ highlighting
  (markdown-hide-urls nil)                  ;; show full URLs, don't collapse them
  :config
  ;; Use pandoc for preview/export when available; harmless if it is not.
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc")))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
