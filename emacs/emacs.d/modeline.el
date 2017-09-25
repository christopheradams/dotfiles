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
        (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
        "/"
        (propertize "%I" 'face 'font-lock-constant-face) ;; size
        "]"

        '(vc-mode vc-mode)

        " "

        ;; the current major mode for the buffer.
        '(:propertize ("" mode-name))

        ;; " --"
        ;; minor-mode-alist  ;; list of minor modes

        'mode-line-misc-info
        'mode-line-end-spaces
        )))
  (setq-default mode-line-format my-mode-line-format))
