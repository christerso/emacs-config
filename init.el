;;; init.el --- Literate Emacs configuration loader
;;; Commentary:
;; This file loads the actual configuration from config.org using org-babel

;;; Code:

;; Load org-mode for literate configuration
(require 'org)

;; Load the literate configuration
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((flycheck-clang-language-standard . "c11")
     (flycheck-gcc-language-standard . "c11") (c-standard . "c11")
     (c-default-style . "gnu") (w32-quote-process-args)
     (comint-process-echoes . t) (explicit-cmdproxy.exe-args "/q")
     (explicit-shell-file-name . "cmdproxy.exe")
     (shell-command-switch . "/c") (shell-file-name . "cmdproxy.exe")
     (gud-gdb-command-name . "C:\\remedybg.exe dream.rdbg")))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "gray" :foreground "black")))))
