;; Start emacs as a server
(server-start)

;;(setq gc-cons-threshold (* 10 1024 1024))

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; custom set-variables and set-faces
(let ((custom.el (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file custom.el)
  (load custom.el))

;;; User
(setq user-mail-address "info@christopheradams.io")

;;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'default-frame-alist '(tty-color-mode . -1))

;;; init.el
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (find-file user-init-file)))
(global-set-key (kbd "C-c I")
                (lambda ()
                  (interactive)
                  (load-file user-init-file)))

;;; Appearance
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)
(setq font-use-system-font t)
;; (menu-bar-mode -1)

;; line numbers
;; (global-linum-mode 1)
;; (add-hook 'org-mode-hook (lambda () (linum-mode 0)))
(setq line-number-mode t)
(setq column-number-mode t)
(setq column-number-indicator-zero-based nil)

(setq ring-bell-function #'ignore)
(setq auto-save-default nil)

(setq-default show-trailing-whitespace t)
(setq sort-fold-case t)
(show-paren-mode t)
(winner-mode 1)
(setq split-window-keep-point nil)

(put 'upcase-region 'disabled nil)

;;; Frames
(add-hook 'after-make-frame-functions 'cxa-contextual-menubar)

;;; Window Resize
(global-set-key (kbd "s-h") 'shrink-window-horizontally)
(global-set-key (kbd "s-l") 'enlarge-window-horizontally)
(global-set-key (kbd "s-a") 'shrink-window)
(global-set-key (kbd "s-z") 'enlarge-window)

;;; Window Navigate
(global-set-key (kbd "s-j") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "s-k") (lambda () (interactive) (other-window -1)))

;;; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))

;;; Formatting
(setq-default indent-tabs-mode nil)
(defun cxa-truncate-no-wrap () (setq truncate-lines t word-wrap nil))
(add-hook 'prog-mode-hook 'cxa-truncate-no-wrap)
(add-hook 'nxml-mode-hook 'cxa-truncate-no-wrap)
(add-hook 'org-mode-hook 'cxa-truncate-no-wrap)
(add-hook 'org-agenda-mode-hook 'cxa-truncate-no-wrap)
(add-hook 'diff-mode-hook (lambda () (setq truncate-lines nil)))

;;; Line wrap fringe bitmaps
(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])
(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])

;;; Dired
;; Hide details by default. Show them with `(`.
(add-hook 'dired-mode-hook
          (function (lambda()
                      (dired-hide-details-mode 1))))

;;; Recentf
(require 'recentf)
(setq recentf-max-saved-items 50
      recentf-max-menu-items 15)
(recentf-mode t)
(add-to-list 'recentf-exclude "\\.emacs.d/")

;;; Tail logs
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;;; Auto Fill Mode (wraps line automatically)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(setq-default fill-column 72)

;;; Text mode shift width
(add-hook 'text-mode-hook
          (function (lambda()
                      (setq evil-shift-width 2))))

;;; Bookmarks
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(setq initial-buffer-choice
      (lambda () (list-bookmarks) (get-buffer "*Bookmark List*")))

;;; Backups
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Lock files
(setq create-lockfiles nil)

;;; SQLi
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;;; C++
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode t)))

;;; css-mode
(add-hook 'css-mode-hook
          (function (lambda()
                      (setq css-indent-offset 2)
                      (setq evil-shift-width css-indent-offset))))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(add-to-list 'auto-mode-alist '("\\.pde$" . java-mode))

;;; xml-mode
(add-to-list `auto-mode-alist '("\\.svg\\'" . xml-mode))

;;; Makefile
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq evil-shift-width 8)))

;;; Regex re-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;;; Functions
(defun cxa-contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

;;; Packages
(straight-use-package 'use-package)

;;; general
(use-package general
  :straight t
  :config
  (general-define-key
   :states '(normal visual emacs motion)
   :keymaps 'override
   :prefix "SPC"
   "" nil
   "<SPC>" 'execute-extended-command
   "0" 'delete-window
   "1" 'delete-other-windows
   "2" 'split-window-below
   "3" 'split-window-right
   "4" 'balance-windows
   ":" 'eval-expression
   "/" 'consult-line
   "<RET>" "<M-return>"
   "a." 'mix-test-current-test
   "ab" 'mix-test-current-buffer
   "af" 'elixir-format
   "ac" 'mix-compile
   "at" 'mix-test
   "ax" 'mix-execute-task ;; C-u C-u SPC a x (choose MIX_ENV and tasks params)
   "b" 'previous-buffer
   "c" 'comment-or-uncomment-region
   "d" 'dired
   "D" 'dired-jump
   "e" 'evil-edit
   "E" (lambda ()
         (interactive)
         (let ((current-prefix-arg '(4))) ; C-u
           (call-interactively #'eshell)))
   "f" 'find-file
   "G" 'magit-blame-toggle
   "g" 'magit-status
   "h" 'evil-window-left
   "I" 'insert-char
   "i" 'fill-paragraph
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
   "m" 'consult-buffer
   "n" 'next-buffer
   "o <RET>" 'org-meta-return
   "o*" 'org-table-iterate
   "o/" 'org-show-todo-tree
   "oa" 'org-agenda
   "ob" 'org-switchb
   "oB" 'org-tree-to-indirect-buffer
   "oc" 'org-capture
   "oD" 'org-dblock-update
   "od" 'org-deadline
   "oe" 'org-set-effort
   "oi" 'org-clock-in
   "oj" 'cxa-org-refile-jump
   "oL" 'org-insert-link
   "ol" 'org-store-link
   "on" 'org-insert-heading-respect-content
   "oN" 'org-add-note
   "oo" 'org-clock-out
   "oq" 'org-set-tags-command
   "or" 'org-reveal
   "os" 'org-schedule
   "oS" 'org-narrow-to-subtree
   "oT" 'cxa-org-todo-done-last-clock-out-time
   "ot" 'org-todo
   "ow" 'cxa-org-refile
   "oW" 'org-refile-goto-last-stored
   "pE" 'project-eshell
   "pf" 'project-find-file
   "pm" 'consult-project-buffer
   "pp" 'project-switch-project
   "pr" 'project-query-replace-regexp
   "ps" 'consult-git-grep
   "pS" 'project-find-regexp
   "R" 're-builder
   "s" 'save-buffer
   "S" 'save-some-buffers
   "T" 'toggle-truncate-lines
   "V" (lambda ()
         (interactive)
         (let ((current-prefix-arg '(4))) ; C-u
           (call-interactively #'vterm)))
   "w" 'widen
   "x" 'eval-defun
   "y" 'consult-yank-from-kill-ring
   "Y" 'cxa-copy-simple))

;;; base16
(use-package base16-theme
  :straight t
  :config
  (load-theme 'base16-grayscale-light t)
  (load-theme 'base16-custom t))

;;; Org-mode
(use-package org
  :straight (:type git :host github :repo "bzg/org-mode"
                   :commit "2afac54c573e373aa77912c5e5c8c9cbe477108e")
  :init
  (require 'ol-bibtex)
  (require 'org-tempo)
  ;; (require 'ox-bibtex) ;; consider org-ref
  :config
  (progn
    (setq org-agenda-remove-tags nil)
    (setq org-agenda-skip-archived-trees t)
    (setq org-agenda-start-with-clockreport-mode t)
    (setq org-agenda-use-time-grid t)
    (setq org-agenda-window-setup 'current-window)
    (setq org-clock-idle-time 10)
    (setq org-clock-in-switch-to-state "CURRENT")
    (setq org-clock-into-drawer t)
    (setq org-clock-out-switch-to-state "STARTED")
    (setq org-clock-x11idle-program-name "xprintidle")
    (setq org-default-priority ?D)
    (setq org-duration-format '(("h" . nil) (special . 2)))
    (setq org-ellipsis "…") ;; … ⤵ ⬎
    (setq org-html-validation-link nil)
    (setq org-log-done t)
    (setq org-log-into-drawer "LOGBOOK")
    (setq org-lowest-priority ?D)
    (setq org-M-RET-may-split-line '((default . nil)))
    (setq org-mouse-1-follows-link nil)
    (setq org-reverse-note-order t)
    (setq org-src-window-setup 'split-window-below)
    (setq org-startup-truncated nil)
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)
    (setq-default org-catch-invisible-edits 'smart)
    (setq org-use-fast-todo-selection 'expert)

    (setq org-export-backends '(ascii html icalendar latex md org))
    (setq org-export-with-sub-superscripts '{})
    (setq org-fontify-done-headline t)
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-fontify-whole-heading-line t)

    ;; indentation
    (setq org-adapt-indentation 'headline-data)
    (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

    ;; Org files
    (setq org-directory '"~/Nextcloud/Org")
    (setq org-agenda-files '("~/Nextcloud/Org"))
    (setq org-default-notes-file (concat
                                  org-directory "/Notes.org"))

    ;; Org refile
    (setq org-refile-targets '((org-agenda-files . (:maxlevel
                                                    . 2))))
    (setq org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)

    ;; Automatically save org buffers after refile
    (advice-add 'org-refile :after
                (lambda (&rest _)
                  (org-save-all-org-buffers)))

    (setq org-file-apps
         (quote ((auto-mode . emacs)
                (directory . emacs)
                ("\\.mm\\'" . default)
                ("\\.x?html?\\'" . browse-url)
                ("\\.pdf\\'" . default))))

    (setq org-capture-templates
          `(("n" "Note" entry (file+olp "" "Inbox" "Notes")
             "* %?\n- %i" :prepend t)
            ("t" "Todo" entry (file+olp "" "Inbox" "Tasks")
             "* TODO %?\n- %i" :prepend t)
            ("i" "Idea" entry (file+olp "" "Inbox" "Ideas")
             "* IDEA %?\n- %i" :prepend t)
            ("d" "Date" entry (file+olp "" "Inbox" "Tasks")
             "* %?%t\n- %i" :prepend t)
            ("m" "Time" entry (file+olp "" "Inbox" "Tasks")
             "* %?%T\n- %i" :prepend t)
            ("r" "Range" entry (file+olp "" "Inbox" "Tasks")
             "* %?%t--%t\n- %i" :prepend t)
            ("l" "Link" entry (file+olp "" "Inbox" "Tasks")
             "* TODO %?\n- %a" :prepend t)
            ("y" "Diary" entry (file+olp ,(concat org-directory "/Journal.org") "Diary" ,(format-time-string "%Y"))
             "* Diary%?%t :taiwan:taipei:\n- %i" :prepend t)))

    (add-hook 'org-capture-before-finalize-hook 'cxa-add-property-with-date-created)

    (setq org-agenda-prefix-format
          (quote
           ((agenda . " %i %-12:c%?-12t% s %b ")
            (todo . " %i %-12:c %b ")
            (tags . " %i %-12:c %b ")
            (search . " %i %-12:c %b "))))

    ;; ‘!’ (for a timestamp) or ‘@’ (for a note with timestamp)
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "IDEA(i)" "CURRENT(c)" "STARTED(s)" "WAITING(w@)" "|" "DONE(d)")
                  (sequence "|" "CANCELLED(x)" "DELEGATED(l@/!)" "DEFERRED(f!)"))))

    (setq org-todo-keyword-faces
          (quote (("TODO" . org-warning)
                  ("NEXT" . org-scheduled-today)
                  ("CURRENT" . org-agenda-current-time)
                  ("WAITING" . org-warning)
                  ("STARTED" . org-warning))))

    ;; agenda clock report
    (setq org-agenda-clockreport-parameter-plist
          (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

    ;; code
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (calc .t)
       (python . t)
       (ruby . t)
       (js . t)
       (shell . t)))
    (setq org-src-fontify-natively t)
    (setq org-confirm-babel-evaluate nil)

    (setq org-babel-python-command "python3")

    ;; bibtex
    (setq bibtex-set-dialect 'biblatex)
    (setq org-bibtex-file "Papers.org")))

(straight-use-package 'org-contrib)

(use-package org-clock-convenience
  :straight t)

(defun cxa-org-refile ()
  "Move the entry or entries at point to another heading."
  (interactive)
  (if (bound-and-true-p org-capture-mode)
      (org-capture-refile)
    (org-refile)))

(defun cxa-org-refile-jump ()
  "Use the refile interface to jump to a heading."
  (interactive)
  (let ((current-prefix-arg '(4))) ;; emulate C-u
    (call-interactively 'org-refile)))

(defun cxa-add-property-with-date-created ()
  "Add CREATED property to the current item."
  (interactive)
  (org-set-property "CREATED" (format-time-string (org-time-stamp-format '(16) t))))

(defun cxa-org-todo-done-last-clock-out-time ()
  "Close the task at the time of the last clock out."
  (interactive)
  (let ((org-use-last-clock-out-time-as-effective-time t))
    (org-todo "DONE")))

(defun cxa-org-export-subtree-to-html-and-open ()
  "Export the current Org subtree to an HTML file and open it."
  (interactive)
  (let ((output-file (org-export-to-file 'html (org-export-output-file-name ".html" t) nil t)))
    (browse-url output-file)))

;;; Column-marker
(use-package column-marker
  :straight t
  :config
  (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80))))

;;; exec-path-from-shell
;;; ensure environment variables match the shell
(use-package exec-path-from-shell
  :straight t
  :init
  (when (memq window-system '(mac ns nil))
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

;;; Emacs libvterm integration
(use-package vterm
  :straight t)

;;; Eat: Emulate A Terminal
(use-package eat
  :straight
  (eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(add-hook 'eat-exec-hook
           (lambda (&rest _)
             (eat-semi-char-mode)
             (evil-insert-state)))

;; Make C-y in Evil insert (or Emacs) always call Eat yank
(with-eval-after-load 'eat
  (define-key eat-semi-char-mode-map (kbd "C-y") #'eat-yank)
  (define-key eat-semi-char-mode-map (kbd "M-y") #'eat-yank-pop))

;;; Undo Fu
(use-package undo-fu
  :straight t)

;;; Evil
(setq evil-want-C-i-jump nil)

(use-package evil
  :straight t
  :init
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode +1)
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-define-key 'insert comint-mode-map
    (kbd "<up>") 'comint-previous-input
    (kbd "<down>") 'comint-next-input)
  (evil-define-key 'insert eat-semi-char-mode-map
    (kbd "C-y") #'eat-yank)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'ag-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs))

;; https://gist.github.com/xahlee/d364cbbff9b3abd12d29
(defun cxa-copy-simple (&optional beg end)
  "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((my-text (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert my-text)
      (goto-char 1)
      (while (looking-at "[ \t\n]")
        (delete-char 1))
      (let ((fill-column 9333999))
        (fill-region (point-min) (point-max)))
      (kill-region (point-min) (point-max)))))

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

;;; Persist history over Emacs restarts.
(use-package savehist
  :init
  (savehist-mode))

;;; VERTical Interactive COmpletion
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :custom
  (vertico-count 10)
  (vertico-multiform-commands
   '((execute-extended-command flat)))
  :init
  (vertico-mode)
  (vertico-multiform-mode))

;;; Orderless completion style
(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Marginalia in the minibuffer
(use-package marginalia
  :straight t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;;; Consulting completing-read
(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode))

;;; Completion Overlay Region FUnction
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-auto-delay 0.25)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (global-corfu-mode))

;;; EditorConfig
(use-package editorconfig
  :straight t
  :config (editorconfig-mode 1))

;;; The Silver Searcher
(use-package ag
  :straight t
  :config
  (setq ag-arguments (quote ("--smart-case" "--stats" "--width" "120")))
  (setq ag-reuse-buffers 't)
  (setq ag-highlight-search t))

;;; ripgrep
(use-package rg
  :straight t
  :config
  (rg-define-search rg-project-wide
    "Search project-wide."
    :files "everything"
    :dir project
    :menu ("Search" "w" "Project-wide")))

;;; Magit
(use-package magit
  :straight t
  :config
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72)
  (setq-default magit-diff-refine-hunk nil)
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (defun magit-blame-toggle()
    (interactive)
    (let* ((active (--filter (and (boundp it) (symbol-value it)) minor-mode-list)))
      (if (member 'magit-blame-mode active)
          (magit-blame-quit)
        (magit-blame))))
  :init
  (add-hook 'magit-blame-mode-hook
            (lambda ()
              (setq magit-blame--style '(headings (heading-format . "%H %-20a %C %s\n")))))
  (add-hook 'magit-diff-mode-hook (lambda () (setq truncate-lines nil)))
  (add-hook 'magit-status-mode-hook (lambda () (setq truncate-lines nil)))
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (global-set-key (kbd "C-x G") 'magit-blame-toggle)
  (global-set-key (kbd "C-x g") 'magit-status))

;;; diff-hl
(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-dired-mode)
  ;;(setq diff-hl-draw-borders nil)
  ;;(diff-hl-margin-mode)
  (setq diff-hl-side 'right)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; nginx
(use-package nginx-mode
  :straight t)

;;; rainbow-mode
(use-package rainbow-mode
  :straight t)

;;; web-mode
(use-package web-mode
  :straight t
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-css-indent t)
  (add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
  (add-to-list 'web-mode-engines-alist '("none" . "\\.html\\'"))
  :init
  (add-hook 'web-mode-hook
            (function (lambda()
                        (setq evil-shift-width web-mode-markup-indent-offset)))))

;;; Yankpad
(use-package yankpad
  :straight t
  :defer 10
  :init
  (setq yankpad-file (concat org-directory "/Yankpad.org")))

;;; Markdown
(use-package markdown-mode
  :straight t
  :config
  (add-hook 'markdown-mode-hook (lambda () (set-fill-column 72)))
  (setq markdown-command "pandoc --from gfm --to html --standalone"))

;;; YAML
(use-package yaml-mode
  :straight t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :hook (yaml-mode . (lambda () (setq-local comment-auto-fill-only-comments t)))
  :config
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map (kbd "RET") 'newline-and-indent))))

;;; SQL
;;; sudo pip install format-sql --prefix='/usr/local'
(use-package format-sql
  :straight t)

(use-package sql-indent
  :straight t
  :init
  (add-hook 'sql-mode-hook
            (lambda () (sqlind-minor-mode))))

;;; TeX
(use-package tex
  :straight auctex
  :init
  (add-hook 'TeX-mode-hook
            (lambda ()
              (setq require-final-newline t)
              (auto-fill-mode -1)))
  :config
  (add-to-list 'auto-mode-alist '("\\.latex\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.fontspec\\'" . LaTeX-mode)))

;;; JSON
(use-package json-mode
  :straight t
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2))))

;;; JavaScript
(use-package js2-mode
  :straight t
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq js-indent-level tab-width)
              (setq evil-shift-width js-indent-level)))
  :config
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode)))

;;; Typescript
(use-package typescript-mode
  :straight t
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq typescript-indent-level tab-width)
              (setq evil-shift-width typescript-indent-level))))

;;; Ruby
(use-package ruby-end
  :straight t
  :defer t)
(use-package ruby-mode
  :straight t
  :init
  (add-hook 'ruby-mode-hook
            (function (lambda()
                        (setq evil-shift-width ruby-indent-level))))
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist
               '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

;;; Erlang
(use-package erlang
  :straight t
  :init
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq erlang-indent-level tab-width))))

;;; Elixir
(use-package elixir-mode
  :straight t
  :config
  (add-to-list 'elixir-mode-hook
               (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                 (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                      "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                 (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                 (ruby-end-mode +1)))
  :init
  (add-hook 'elixir-mode-hook
            (function (lambda()
                        (setq evil-shift-width elixir-smie-indent-basic)))))

(use-package mix
  :straight t
  :config
  (add-hook 'elixir-mode-hook 'mix-minor-mode))

;;; Haskell
(use-package haskell-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.xmobarrc\\'" . haskell-mode)))

;;; Go
(use-package go-mode
  :straight t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4))))

;;; PHP
(use-package php-mode
  :straight t)

;;; GraphQL
(use-package graphql-mode
  :straight t)

;;; Solidity
(use-package solidity-mode
  :straight t
  :init
  (add-hook 'solidity-mode-hook
            (lambda ()
              (setq c-basic-offset 4
                    tab-width 4
                    indent-tabs-mode nil)))
  :config
  (setq solidity-comment-style 'slash))

;; (init-open-recentf)
(put 'downcase-region 'disabled nil)

;;; mode-line
(let ((modeline.el (expand-file-name "modeline.el" user-emacs-directory)))
  (load modeline.el))

;;; Secrets
(let ((secret.el (expand-file-name "secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))
