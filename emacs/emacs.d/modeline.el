(defvar ml-selected-window nil)

(defun ml-record-selected-window ()
  (setq ml-selected-window (selected-window)))

(defun ml-update-all ()
  (force-mode-line-update t))

(add-hook 'post-command-hook 'ml-record-selected-window)

(add-hook 'buffer-list-update-hook 'ml-update-all)

;; Make modeline look inactive when frame loses focus
(add-hook 'focus-out-hook
          (lambda ()
            (copy-face 'mode-line '--mode-line-backup)
            (copy-face 'mode-line-inactive 'mode-line)))
(add-hook 'focus-in-hook
          (lambda ()
            (copy-face '--mode-line-backup 'mode-line)))

(let ((my-mode-line-format
       (list

        "[" ;; projectile
        '(:eval (projectile-project-name))
        "]"

        ;; is this buffer read-only?
        '(:eval (when buffer-read-only
                  (concat " "  (propertize "RO"
                                           'face 'font-lock-type-face
                                           'help-echo "Buffer is read-only"))))

        ;;; is buffer modified?
        '(:eval (when (buffer-modified-p)
                  (concat " "  (propertize "***"
                                           'face 'font-lock-warning-face
                                           'help-echo "Buffer has been modified"))))

        '(:eval
          (let* ((code (symbol-name buffer-file-coding-system))
                 (eol-type (coding-system-eol-type buffer-file-coding-system))
                 (eol (if (eq 0 eol-type) ""
                        (if (eq 1 eol-type)
                            (concat " " (propertize "DOS" 'face 'font-lock-warning-face))
                          (if (eq 2 eol-type)
                              (concat " " (propertize "MAC" 'face 'font-lock-warning-face))
                            (propertize "???" 'face 'font-lock-warning-face))))))
            eol))

        " "

        ;; the buffer name; the file name as a tool tip
        '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                      'help-echo (buffer-file-name)))

        ;; line and column
        "(" ;; '%02' to set to 2 chars at least; prevents flickering
        (propertize "%02l" 'face 'font-lock-type-face) ","
        (propertize "%02c" 'face 'font-lock-type-face)
        ") "

        ;; relative position, size of file
        "["
        (propertize "%p" 'face 'font-lock-type-face) ;; % above top
        "/"
        (propertize "%I" 'face 'font-lock-type-face) ;; size
        "]"

        '(vc-mode vc-mode)

        " "

        ;; the current major mode for the buffer.
        '(:propertize ("" mode-name))

        ;; " --"
        ;; minor-mode-alist  ;; list of minor modes

        '(:eval
          (if (eq ml-selected-window (selected-window))
              'mode-line-misc-info))

        'mode-line-end-spaces
        )))
  (setq-default mode-line-format my-mode-line-format))
