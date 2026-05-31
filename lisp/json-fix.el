;;; json-fix.el --- Validate JSON and offer to auto-repair it -*- lexical-binding: t; -*-
;;; Commentary:
;; `cs/json-fix' checks the current buffer as JSON.  If it is broken it runs the
;; `jsonrepair' CLI (https://github.com/josdejong/jsonrepair) and, when that
;; yields valid JSON, ASKS before replacing the buffer with the repaired text.
;;
;; jsonrepair handles the common breakages — trailing commas, missing quotes,
;; single quotes, comments, unquoted keys, Python None/True/False — but some
;; errors are genuinely ambiguous (e.g. a missing `]' right before a new key);
;; for those it cannot repair, this reports the error and moves point to the
;; broken spot so you can fix it by hand.
;;
;; Bound to C-c C-f in the JSON modes; also available as `M-x cs/json-fix'.
;;; Code:

(defun cs/json--valid-p (string)
  "Return non-nil if STRING parses as valid JSON."
  (condition-case nil
      (progn (json-parse-string string) t)
    (error nil)))

(defun cs/json--repair (string)
  "Run jsonrepair on STRING.
Return (t . REPAIRED) when it produces valid JSON, else (ERROR-STRING . nil)."
  (let ((errfile (make-temp-file "jsonrepair-err")))
    (unwind-protect
        (with-temp-buffer
          (insert string)
          (let* ((code (call-process-region (point-min) (point-max) "jsonrepair"
                                            t (list t errfile) nil))
                 (out  (buffer-string))
                 (err  (with-temp-buffer
                         (insert-file-contents errfile)
                         (string-trim (buffer-string)))))
            (if (and (zerop code) (cs/json--valid-p out))
                (cons t out)
              (cons (if (string-empty-p err) "could not repair" err) nil))))
      (delete-file errfile))))

(defun cs/json-fix ()
  "Validate the current buffer as JSON; if broken, offer to auto-repair it.
Asks before changing anything.  If jsonrepair cannot fix the document, jump
point to the reported error position so it can be fixed manually."
  (interactive)
  (let ((src (buffer-string)))
    (cond
     ((string-empty-p (string-trim src))
      (message "Buffer is empty."))
     ((cs/json--valid-p src)
      (message "JSON is valid — nothing to fix."))
     ((not (executable-find "jsonrepair"))
      (user-error "jsonrepair not on PATH — install with: npm install -g jsonrepair"))
     (t
      (pcase-let ((`(,result . ,repaired) (cs/json--repair src)))
        (cond
         ;; jsonrepair produced valid JSON — ask, then apply.
         ((eq result t)
          (if (y-or-n-p "JSON has fixable issues. Apply auto-fix? ")
              (let ((pt (point)))
                (atomic-change-group
                  (erase-buffer)
                  (insert repaired))
                (goto-char (min pt (point-max)))
                (message "JSON auto-fixed. Save to format via the language server."))
            (message "Left unchanged.")))
         ;; Not repairable — report and jump to the broken spot.
         (t
          (let ((pos (and (string-match "position \\([0-9]+\\)" result)
                          (string-to-number (match-string 1 result)))))
            (when pos (goto-char (min (1+ pos) (point-max))))
            (user-error "Can't auto-fix (%s)%s"
                        result
                        (if pos " — point moved to the problem" ""))))))))))

(with-eval-after-load 'json-ts-mode
  (define-key json-ts-mode-map (kbd "C-c C-f") #'cs/json-fix))
(with-eval-after-load 'js
  (when (boundp 'js-json-mode-map)
    (define-key js-json-mode-map (kbd "C-c C-f") #'cs/json-fix)))

(provide 'json-fix)
;;; json-fix.el ends here
