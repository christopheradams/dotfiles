;;; gptel-tools
(with-eval-after-load 'gptel
  (gptel-make-tool
  :name "toggle_appearance"
  :function
  (lambda (request)
    (unless (stringp request)
      (error "error: request must be a string"))
    (auto-dark-toggle-appearance)
    (format "Toggled auto-dark appearance in response to request: %s" request))
  :description "Toggle Emacs appearance using auto-dark. Use this for requests to toggle appearance, light mode, dark mode, auto-dark mode, or theme appearance. Any phrasing like 'toggle dark mode', 'toggle light mode', or 'toggle auto-dark' should call this tool."
  :args
  (list
    '(:name "request"
      :type string
      :description "The user's request text, such as 'toggle dark mode', 'toggle light mode', or 'toggle appearance'." ))
  :category "emacs"))
