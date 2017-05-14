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
        "] "

        ;; the current major mode for the buffer.
        '(:propertize ("" mode-name))

        '(vc-mode vc-mode)

        'mode-line-misc-info
        'mode-line-end-spaces

        ;; " --"
        ;; minor-mode-alist  ;; list of minor modes
        )))
  (setq-default mode-line-format my-mode-line-format))
