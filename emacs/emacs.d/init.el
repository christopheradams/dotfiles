;; Start emacs as a server
(server-start)

;;(setq gc-cons-threshold (* 10 1024 1024))

;; custom set-variables and set-faces
(let ((custom.el (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file custom.el)
  (load custom.el))

;;; User
(setq user-mail-address "email@christopheradams.io")

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
(setq inhibit-startup-screen t)
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
(show-paren-mode t)
(winner-mode 1)

(put 'upcase-region 'disabled nil)

;;; Window Resize
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;;; Scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))

;;; Formatting
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook '(lambda ()
    (setq truncate-lines t
          word-wrap nil)))
(add-hook 'nxml-mode-hook '(lambda ()
    (setq truncate-lines t
          word-wrap nil)))
(add-hook 'org-mode-hook '(lambda ()
    (setq truncate-lines t
          word-wrap nil)))
(add-hook 'org-agenda-mode-hook '(lambda ()
    (setq truncate-lines t
          word-wrap nil)))
(add-hook 'epresent-mode-hook '(lambda ()
    (setq truncate-lines nil
          word-wrap t)))

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
(setq-default fill-column 80)

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

;;; Secrets
(let ((secret.el (expand-file-name "secret.el" user-emacs-directory)))
  (when (file-exists-p secret.el)
    (load secret.el)))

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

;;; Packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

;;; general
(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual emacs motion)
   :keymaps 'override
   :prefix "SPC"
   "" nil
   "<SPC>" 'smex
   "1" 'delete-other-windows
   "2" 'split-window-below
   "3" 'split-window-right
   "0" 'delete-window
   "=" 'balance-windows
   "d" 'dired
   "e" 'evil-edit
   "f" 'find-file
   "i" 'fill-paragraph
   "I" 'counsel-unicode-char
   "h" 'evil-window-left
   "j" 'evil-window-down
   "k" 'evil-window-up
   "l" 'evil-window-right
   "b" 'previous-buffer
   "n" 'next-buffer
   "m" 'switch-to-buffer
   "s" 'save-buffer
   "S" 'save-some-buffers
   "g" 'magit-status
   "G" 'magit-blame-toggle
   "oa" 'org-agenda
   "oc" 'org-capture
   "ob" 'org-iswitchb
   "od" 'org-deadline
   "os" 'org-schedule
   "oD" 'org-dblock-update
   "ol" 'org-store-link
   "oL" 'org-insert-link
   "oi" 'org-clock-in
   "on" 'org-insert-todo-heading-respect-content
   "oo" 'org-clock-out
   "ot" 'org-todo
   "oT" 'cxa-org-todo-done-last-clockout-time
   "o/" 'org-show-todo-tree
   "oq" 'org-set-tags
   "ow" 'cxa-org-refile
   "oj" 'counsel-org-goto
   "o*" 'org-table-recalculate
   "pp" 'projectile-switch-project
   "pf" 'projectile-find-file
   "pm" 'projectile-switch-to-buffer
   "pr" 'projectile-replace
   "ps" 'counsel-projectile-ag
   "pS" 'projectile-ag
   "yi" 'yankpad-insert
   "yc" 'yankpad-capture-snippet
   "yy" 'yankpad-set-category
   "af" 'elixir-format
   "ax" 'alchemist-mix
   "at" 'alchemist-mix-test
   "a." 'alchemist-mix-test-at-point
   "amc" 'alchemist-mix-compile))

;;; base16
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-grayscale-light t)
  (load-theme 'base16-custom t))

;;; Org-mode
(use-package org
  :ensure org-plus-contrib
  :init
  (require 'org-bibtex)
  (require 'ox-bibtex)
  :config
  (progn
    (setq org-log-done t)
    (setq org-agenda-files '("~/Dropbox/Org"))
    (setq org-directory '"~/Dropbox/Org")
    (setq org-default-notes-file (concat org-directory "/Notes.org"))
    (setq org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((org-agenda-files . (:maxlevel . 2))))
    (setq org-reverse-note-order t)
    (setq org-clock-in-switch-to-state "CURRENT")
    (setq org-clock-out-switch-to-state "STARTED")
    (setq org-duration-format '(("h" . nil) (special . 2)))
    (setq org-clock-idle-time 20)

    ;; Keep mouse-1 clicks from following a link
    (setq org-mouse-1-follows-link nil)

    ;; Separate drawers for clocking and logs
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)

    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "" "Tasks")
             "* TODO %?\n  %i")
            ("i" "Idea" entry (file+headline "" "Tasks")
             "* IDEA %?\n  %i")
            ("d" "Date" entry (file+headline "" "Tasks")
             "* %?%t\n  %i")
            ("m" "Time" entry (file+headline "" "Tasks")
             "* %?%T\n  %i")
            ("r" "Range" entry (file+headline "" "Tasks")
             "* %?%t--%t\n  %i")
            ("l" "Link" entry (file+headline "" "Tasks")
             "* TODO %?\n  %a")))

    (add-hook 'org-capture-before-finalize-hook 'cxa-add-property-with-date-created)

    (setq org-agenda-prefix-format
          (quote
           ((agenda . " %i %-12:c%?-12t% s")
            (timeline . "  % s")
            (todo . " %i %-12:c")
            (tags . " %i %-12:c")
            (search . " %i %-12:c"))))
    (setq org-agenda-remove-tags nil)
    (setq org-agenda-skip-archived-trees t)
    (setq org-agenda-use-time-grid t)
    (setq org-startup-truncated nil)
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-start-with-clockreport-mode t)

    ;; Automatically save org buffers after refile
    (advice-add 'org-refile :after
                (lambda (&rest _)
                  (org-save-all-org-buffers)))

    ;; ‘!’ (for a timestamp) or ‘@’ (for a note with timestamp)
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "IDEA(i)" "NEXT(n)" "CURRENT(c)" "STARTED(s)" "|" "DONE(d)")
                  (sequence "|" "WAITING(w@/!)" "CANCELLED(x)" "DELEGATED(l@/!)" "DEFERRED(f!)"))))

    (setq org-todo-keyword-faces
          (quote (("TODO" . org-warning)
                  ("NEXT" . org-scheduled-today)
                  ("CURRENT" . org-priority)
                  ("WAITING" . org-priority)
                  ("STARTED" . org-warning))))

    (setq org-treat-S-cursor-todo-selection-as-state-change nil)

    ;; agenda clock report
    (setq org-agenda-clockreport-parameter-plist
          (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

    ;; code
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (calc .t)
       (ruby . t)
       (js . t)
       (shell . t)))
    (setq org-src-fontify-natively t)
    (defun cxa-org-confirm-babel-evaluate (lang body)
      (not (member lang '("emacs-lisp"))))
    (setq org-confirm-babel-evaluate 'cxa-org-confirm-babel-evaluate)

    ;; bibtex
    (setq bibtex-set-dialect 'biblatex)
    (setq org-bibtex-file "Papers.org")))

(use-package org-clock-convenience
  :ensure t)
(use-package epresent
  :ensure t
  :config
  (add-hook 'epresent-mode-hook (lambda () (setq show-trailing-whitespace nil))))

(defun cxa-org-refile ()
  "Move the entry or entries at point to another heading."
  (interactive)
  (if (bound-and-true-p org-capture-mode)
      (org-capture-refile)
    (org-refile)))

(defun cxa-add-property-with-date-created ()
  "Add CREATED property to the current item."
  (interactive)
  (org-set-property "CREATED" (format-time-string (org-time-stamp-format '(16) t))))

(defun cxa-org-todo-done-last-clockout-time ()
    "Close the task at the time of the last clock out."
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      (when (re-search-forward org-clock-convenience-tr-re nil t)
        (let ((last-clock-out (match-string 9)))
          (org-back-to-heading t)
          (org-todo "DONE")
          (re-search-forward org-closed-time-regexp)
          (re-search-backward org-ts-regexp1)
          (replace-match last-clock-out)))))

;;; Column-marker
(use-package column-marker
  :ensure t
  :config
  (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80))))

;;; exec-path-from-shell
;;; ensure environment variables match the shell
(use-package exec-path-from-shell
  :ensure t
  :init
 (when (memq window-system '(mac ns nil))
    (exec-path-from-shell-initialize)))

;;; Evil
(setq evil-want-C-i-jump nil)

(use-package evil
  :ensure t
  :init (evil-mode +1)
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (evil-define-key 'insert comint-mode-map
    (kbd "<up>") 'comint-previous-input
    (kbd "<down>") 'comint-next-input)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'epresent-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs))

;;; Smex
(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-global-mode +1)
  :config (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
    :ensure t)

;;; Ivy
(use-package ivy
  :ensure t
  :config (ivy-mode 1))

;;; EditorConfig
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;;; ibuffer-vc
(use-package ibuffer-vc
  :ensure t
  :init
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;; Company
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;; Ag
(use-package ag
  :ensure t
  :config
  (setq ag-arguments (quote ("--smart-case" "--stats" "--width" "120")))
  (setq ag-reuse-buffers 't)
  (setq ag-highlight-search t))

;;; Magit
(use-package magit
  :ensure t
  :config
  (setq git-commit-summary-max-length 50
        git-commit-fill-column 72)
  (defun magit-blame-toggle()
    (interactive)
    (let* ((active (--filter (and (boundp it) (symbol-value it)) minor-mode-list)))
      (if (member 'magit-blame-mode active)
          (magit-blame-quit)
        (magit-blame ))))
  :init
  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (global-set-key (kbd "C-x G") 'magit-blame-toggle)
  (global-set-key (kbd "C-x g") 'magit-status))

;;; diff-hl
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  :config
  (diff-hl-dired-mode)
  ;;(setq diff-hl-draw-borders nil)
  ;;(diff-hl-margin-mode)
  (setq diff-hl-side 'right)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;; hlinum-mode
(use-package hlinum
  :ensure t
  :init
  (hlinum-activate))

;;; restclient
(use-package restclient
  :ensure t)
(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

;;; know-your-http-well
(use-package know-your-http-well
  :ensure t)

;;; gscholar bibtex
(use-package gscholar-bibtex
  :ensure t)

;;; rainbow-mode
(use-package rainbow-mode
  :ensure t)

;;; css-mode
(add-hook 'css-mode-hook
          (function (lambda()
                      (setq css-indent-offset 2)
                      (setq evil-shift-width css-indent-offset))))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;;; web-mode
(use-package web-mode
  :ensure t
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook
            (function (lambda()
                        (setq evil-shift-width web-mode-markup-indent-offset)))))

;;; Yankpad
(use-package yankpad
  :ensure t
  :defer 10
  :init
  (setq yankpad-file (concat org-directory "/Yankpad.org"))
  (add-to-list 'company-backends #'company-yankpad))

;;; Markdown
(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-command "pandoc --from markdown_github --to html --standalone"))

;;; YAML
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;;; Slim
(use-package slim-mode
  :ensure t
  :init
  (add-hook 'slim-mode-hook
            (function (lambda()
                        (setq evil-shift-width 2))))
  :config
  (add-to-list 'auto-mode-alist '("\\.slim\\'" . slim-mode)))

;;; SQL
;;; sudo pip install format-sql --prefix='/usr/local'
(use-package format-sql
  :ensure t)

(use-package sql-indent
  :ensure t
  :init
  (add-hook 'sql-mode-hook
            (lambda () (sqlind-minor-mode))))

;;; TeX
(use-package tex
  :ensure auctex)

;;; JavaScript
(use-package js2-mode
  :ensure t
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

;;; Ruby
(use-package ruby-end
  :ensure t
  :defer t)
(use-package ruby-mode
  :ensure t
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
  :ensure t
  :init
  (add-hook 'erlang-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq erlang-indent-level tab-width))))

;;; Elixir
(use-package elixir-mode
  :ensure t
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
(use-package alchemist
  :ensure t)

;;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.xmobarrc\\'" . haskell-mode)))

;;; Go
(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4))))

;;; PHP
(use-package php-mode
  :ensure t)

;;; GraphQL
(use-package graphql-mode
  :ensure t)

;;; Solidity
(use-package solidity-mode
  :ensure t
  :config
  (setq solidity-comment-style 'slash))


;; (init-open-recentf)
(put 'downcase-region 'disabled nil)

;;; mode-line
(let ((modeline.el (expand-file-name "modeline.el" user-emacs-directory)))
    (load modeline.el))
