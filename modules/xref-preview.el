;; Custom xref show function with preview
(defun xref-show-xrefs-function-with-preview (fetcher alist)
  "Custom xref show function with preview capabilities."
  (let ((xrefs (funcall fetcher)))
    (if (not xrefs)
        (user-error "No references found")
      (xref-show-xrefs-with-preview xrefs alist))))

;; Interactive xref with preview
(defun xref-find-references-with-preview ()
  "Find references with interactive preview."
  (interactive)
  (let ((xref-show-xrefs-function 'xref-show-xrefs-function-with-preview))
    (call-interactively 'xref-find-references)))

(defun xref-find-definitions-with-preview ()
  "Find definitions with interactive preview."
  (interactive)
  (let ((xref-show-xrefs-function 'xref-show-xrefs-function-with-preview))
    (call-interactively 'xref-find-definitions)))

(defun xref-show-xrefs-with-preview (xrefs alist)
  "Show xrefs in a buffer with interactive preview."
  (let ((buf (get-buffer-create "*xref-preview*"))
        (original-window (selected-window))
        (original-buffer (current-buffer)))

    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Navigate with ↑/↓, jump with RET, quit with q\n")
      (insert "─────────────────────────────────────────────────\n\n")

      ;; Store data as buffer-local variables
      (setq-local xref-preview-list xrefs)
      (setq-local xref-preview-index 0)
      (setq-local xref-preview-original-window original-window)
      (setq-local xref-preview-original-buffer original-buffer)
      (setq-local xref-preview-current-overlay nil)

      ;; Prevent auto-focus interference
      (setq-local auto-focus-disabled t)

      ;; Insert xref entries
      (dolist (xref xrefs)
        (let* ((summary (xref-item-summary xref))
               (location (xref-item-location xref))
               (file (xref-location-group location))
               (line (xref-location-line location)))
          (insert (format "%s:%d: %s\n"
                         (file-name-nondirectory file)
                         line
                         summary))))

      (goto-char (point-min))
      (forward-line 3) ; Skip header

      ;; Clear any selection in the preview buffer itself
      (deactivate-mark t)
      (setq mark-active nil)
      (push-mark (point) nil nil) ; Set mark but don't activate
      (deactivate-mark t)

      (setq buffer-read-only t)

      ;; Define local keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<up>") 'xref-preview-up)
        (define-key map (kbd "<down>") 'xref-preview-down)
        (define-key map (kbd "p") 'xref-preview-up)
        (define-key map (kbd "n") 'xref-preview-down)
        (define-key map (kbd "<return>") 'xref-preview-jump)
        (define-key map (kbd "RET") 'xref-preview-jump)
        (define-key map (kbd "q") 'xref-preview-quit)
        (define-key map (kbd "C-g") 'xref-preview-quit)
        (define-key map (kbd "<escape>") 'xref-preview-quit)
        (use-local-map map))

      ;; Also set buffer-local quit function for emergency
      (setq-local quit-window-hook 'xref-preview-quit))

    ;; Display the buffer in bottom window
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 15))))
    (select-window (get-buffer-window buf))

    ;; Show initial preview in main window
    (xref-preview-update-display)

    ;; Aggressively clear any selection mode
    (with-current-buffer buf
      (deactivate-mark)
      (setq mark-active nil)
      (setq-local transient-mark-mode nil)
      (setq-local cua-mode nil)
      (goto-char (point-min))
      (forward-line 3)
      (set-mark (point))
      (deactivate-mark))))

(defun xref-preview-up ()
  "Move to previous xref entry."
  (interactive)
  (when (> xref-preview-index 0)
    (setq xref-preview-index (1- xref-preview-index))
    (xref-preview-update-display)))

(defun xref-preview-down ()
  "Move to next xref entry."
  (interactive)
  (when (< xref-preview-index (1- (length xref-preview-list)))
    (setq xref-preview-index (1+ xref-preview-index))
    (xref-preview-update-display)))

(defun xref-preview-jump ()
  "Jump to selected xref entry."
  (interactive)
  (let ((xref (nth xref-preview-index xref-preview-list))
        (orig-win xref-preview-original-window)
        (current-win (selected-window))
        (current-buf (current-buffer))
        (current-overlay xref-preview-current-overlay))

    ;; Clean up highlight overlay
    (when (and current-overlay (overlay-buffer current-overlay))
      (delete-overlay current-overlay))

    ;; Kill the preview buffer
    (set-buffer-modified-p nil)
    (kill-buffer current-buf)

    ;; Close the preview window
    (when (> (length (window-list)) 1)
      (delete-window current-win))

    ;; Jump to the location in the main window
    (when (window-live-p orig-win)
      (select-window orig-win))
    (xref-show-location (xref-item-location xref) 'window)))

(defun xref-preview-quit ()
  "Quit xref preview with aggressive cleanup."
  (interactive)
  (message "Attempting to quit xref preview...")
  (let ((orig-buf xref-preview-original-buffer)
        (orig-win xref-preview-original-window)
        (current-buf (current-buffer))
        (current-win (selected-window))
        (current-overlay xref-preview-current-overlay))

    ;; Clean up highlight overlay
    (when (and current-overlay (overlay-buffer current-overlay))
      (delete-overlay current-overlay))

    ;; Force kill the current buffer
    (set-buffer-modified-p nil)
    (kill-buffer current-buf)

    ;; Close the window if it's not the only one
    (when (> (length (window-list)) 1)
      (delete-window current-win))

    ;; Try to restore original state
    (condition-case err
        (progn
          (when (window-live-p orig-win)
            (select-window orig-win))
          (when (buffer-live-p orig-buf)
            (switch-to-buffer orig-buf))
          (message "Xref preview closed successfully"))
      (error
       (message "Xref preview closed with warnings: %s" err)))))

(defun xref-preview-update-display ()
  "Update the display and preview."
  (let ((buf (current-buffer)))
    ;; Update selection highlighting
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (forward-line 3) ; Skip header

      ;; Remove previous highlighting
      (remove-overlays (point) (point-max) 'face)

      ;; Add highlighting to current line
      (forward-line xref-preview-index)
      (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
        (overlay-put overlay 'face 'highlight))

      (setq buffer-read-only t))

    ;; Update preview in main window
    (when (< xref-preview-index (length xref-preview-list))
      (xref-preview-show-location-in-main (nth xref-preview-index xref-preview-list) xref-preview-original-window))))

(defun xref-preview-show-location-in-main (xref original-window)
  "Show location in the main window."
  (let* ((location (xref-item-location xref))
         (file (xref-location-group location))
         (line (xref-location-line location))
         (preview-buf (get-buffer "*xref-preview*")))

    (with-selected-window original-window

      ;; Clean up previous overlay first
      (when (and preview-buf
                 (buffer-local-value 'xref-preview-current-overlay preview-buf))
        (let ((old-overlay (buffer-local-value 'xref-preview-current-overlay preview-buf)))
          (when (and old-overlay (overlay-buffer old-overlay))
            (delete-overlay old-overlay))))

      (find-file file)
      (goto-char (point-min))
      (forward-line (1- line))
      (recenter)

      ;; Create highlight overlay for the line in main window
      (let ((overlay (make-overlay (line-beginning-position) (line-end-position))))
        (overlay-put overlay 'face '(:background "#3a3a3a" :foreground "white"))

        ;; Store the overlay reference in the preview buffer for cleanup
        (when preview-buf
          (with-current-buffer preview-buf
            (setq-local xref-preview-current-overlay overlay)))))))

;; Emergency xref cleanup function
(defun xref-preview-force-quit ()
  "Force quit any stuck xref preview buffer."
  (interactive)
  (let ((xref-buf (get-buffer "*xref-preview*")))
    (when xref-buf
      (with-current-buffer xref-buf
        (set-buffer-modified-p nil))
      (kill-buffer xref-buf)
      (message "Force-killed xref preview buffer"))))

;; Emergency keybinding
(global-set-key (kbd "C-c x q") 'xref-preview-force-quit)

;; Force override rectangle keymap by setting the keymap to nil first
(define-key ctl-x-map "r" nil)

;; Xref navigation keybindings
(global-set-key (kbd "C-x r") 'xref-find-references-with-preview)
(global-set-key (kbd "C-x g") 'xref-find-definitions-with-preview)

;; Alternative keybindings - use C-x prefix for xref to avoid conflicts
(global-set-key (kbd "C-x g") 'xref-find-definitions-with-preview)
