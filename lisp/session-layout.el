;;; session-layout.el --- Reopen frames with the exact window splits + buffers -*- lexical-binding: t; -*-
;;; Commentary:
;; The daemon keeps every buffer alive between `emacsclient -c' sessions, but a
;; brand-new client frame always opens as a single window on *scratch* — the
;; split layout of the frame you just closed is gone.  desktop.el (configured in
;; config.org) restores the *buffer list*; this module restores how those
;; buffers were *arranged*: the window splits, which buffer sat in each window,
;; point in each, and the frame's size.
;;
;; Two moments are covered:
;;   * Close + reopen while the daemon stays up — captured on `delete-frame' and
;;     replayed into the next client frame (buffers are all still live).
;;   * Full daemon restart — the layout is also written to disk on shutdown and
;;     read back when the first client frame appears.
;;
;; Restoration only fires for the FIRST graphical frame; additional frames you
;; open afterwards start fresh, so this never clobbers a second window you opened
;; on purpose.
;;; Code:

(defvar cs/session-layout-file
  (expand-file-name "last-session-layout.el" user-emacs-directory)
  "File holding the most recent window layout, for restore across daemon restarts.")

(defvar cs/session-layout nil
  "In-memory snapshot of the last graphical frame's window layout.
A plist: (:windows WINDOW-STATE :width COLS :height ROWS).")

(defun cs/session--real-graphical-frame-p (frame)
  "Non-nil when FRAME is a graphical frame the user actually works in.
Child frames (corfu/posframe popups) are graphical too and pass through
`delete-frame-functions' constantly; capturing one would overwrite the
saved layout with a popup-sized stamp."
  (and (display-graphic-p frame)
       (not (frame-parameter frame 'parent-frame))))

(defun cs/session--graphical-frame ()
  "Return some live graphical frame, or nil."
  (seq-find #'cs/session--real-graphical-frame-p (frame-list)))

(defun cs/session-capture (&optional frame)
  "Snapshot FRAME's window layout and outer size into `cs/session-layout'.
FRAME defaults to a live graphical frame.  Called from `delete-frame-functions'
with the dying frame, so the layout is grabbed just before it disappears."
  (let ((frame (or frame (cs/session--graphical-frame))))
    (when (and frame (frame-live-p frame)
               (cs/session--real-graphical-frame-p frame))
      (setq cs/session-layout
            (list :windows (window-state-get (frame-root-window frame) t)
                  :width   (frame-width frame)
                  :height  (frame-height frame))))))

(defun cs/session-persist ()
  "Capture the current layout and write it to `cs/session-layout-file'."
  (cs/session-capture)
  (when cs/session-layout
    (ignore-errors
      (with-temp-file cs/session-layout-file
        (let ((print-level nil) (print-length nil))
          (prin1 cs/session-layout (current-buffer)))))))

(defun cs/session-load-from-disk ()
  "Populate `cs/session-layout' from disk if it is not already set in memory."
  (when (and (null cs/session-layout)
             (file-readable-p cs/session-layout-file))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents cs/session-layout-file)
        (setq cs/session-layout (read (current-buffer)))))))

(defun cs/session-restore ()
  "Restore the saved layout into the current frame, if it is the first one."
  (when (and (cs/session--real-graphical-frame-p (selected-frame))
             (= 1 (seq-count #'cs/session--real-graphical-frame-p (frame-list))))
    (cs/session-load-from-disk)
    (when cs/session-layout
      (let ((w  (plist-get cs/session-layout :width))
            (h  (plist-get cs/session-layout :height))
            (st (plist-get cs/session-layout :windows)))
        ;; Sanity floor: a saved size smaller than this is popup garbage,
        ;; not a layout anyone worked in — never shrink the frame to it.
        (when (and w h (>= w 40) (>= h 12))
          (ignore-errors (set-frame-size (selected-frame) w h)))
        (when st (ignore-errors
                   (window-state-put st (frame-root-window) 'safe)))
        (ignore-errors (redraw-frame))))))

;; Capture whenever a graphical frame is closed (C-x 5 0, or the WM close button).
(add-hook 'delete-frame-functions #'cs/session-capture)
;; Persist on shutdown so a daemon restart can reopen the same arrangement.
(add-hook 'kill-emacs-hook #'cs/session-persist)
;; Restore into the next client frame.
(add-hook 'server-after-make-frame-hook #'cs/session-restore)

(provide 'session-layout)
;;; session-layout.el ends here
