;; Start emacs as a server
(server-start)

;;(setq gc-cons-threshold (* 10 1024 1024))

;; custom set-variables and set-faces
(let ((custom.el (expand-file-name "custom.el" user-emacs-directory)))
  (setq custom-file custom.el)
  (load custom.el))

;;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'default-frame-alist '(tty-color-mode . -1))
(if (display-graphic-p)
  (load-theme 'base16-grayscale-light t)

  ;; Make sure the theme is loaded after frame creation
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame
                    (load-theme 'base16-grayscale-light t))))
    (load-theme 'base16-grayscale-light t)))

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

(setq ring-bell-function #'ignore)
(setq auto-save-default nil)

(setq-default show-trailing-whitespace t)
(show-paren-mode t)

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

;;; Org-mode
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
(setq org-time-clocksum-use-fractional t)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Keep mouse-1 clicks from following a link
(setq org-mouse-1-follows-link nil)

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "" "Tasks")
         "* TODO %?\n  %u\n  %i")
        ("i" "Idea" entry (file+headline "" "Tasks")
         "* IDEA %?\n  %u\n  %i")
        ("l" "Link" entry (file+headline "" "Tasks")
         "* TODO %?\n  %u\n  %a")))

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

;; Automatically save org buffers after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
            (org-save-all-org-buffers)))

;; ‘!’ (for a timestamp) or ‘@’ (for a note with timestamp)
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "IDEA(i)" "NEXT(n)" "CURRENT(c)" "STARTED(s)" "|" "DONE(d)")
              (sequence "|" "WAITING(w@/!)" "CANCELLED(x@/!)" "DELEGATED(l@/!)" "DEFERRED(f!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" . org-warning)
              ("NEXT" . org-warning)
              ("CURRENT" . org-priority)
              ("WAITING" . org-priority)
              ("STARTED" . org-warning))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-agenda-custom-commands
      '(("W" "Completed and/or deferred tasks from previous week"
       ((agenda "" ((org-agenda-span 7)
            (org-agenda-start-day "-7d")
            (org-agenda-entry-types '(:timestamp))
            (org-agenda-prefix-format (quote ((agenda . " %i %-12:c") (timeline . "  % s") (todo . " %i %-12:c") (tags . " %i %-12:c") (search . " %i %-12:c"))))
            (org-agenda-show-log t)))))))

;; agenda clock report
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

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
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

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
(setq evil-leader/no-prefix-mode-rx '("magit-.*-mode" "org-.*-mode" "dired-mode" "bookmark-bmenu-mode"))
(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'smex
    "1" 'delete-other-windows
    "2" 'split-window-below
    "3" 'split-window-right
    "0" 'delete-window
    "=" 'balance-windows
    "d" 'dired
    "f" 'find-file
    "i" 'fill-paragraph
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
    "oa" 'org-agenda
    "oc" 'org-capture
    "ob" 'org-iswitchb
    "ol" 'org-store-link
    "pp" 'projectile-switch-project
    "pf" 'projectile-find-file
    "pm" 'projectile-switch-to-buffer
    "ps" 'counsel-projectile-ag
    "pS" 'projectile-ag
    "at"  'alchemist-mix-test
    "amc" 'alchemist-mix-compile))
(use-package evil
  :ensure t
  :init (evil-mode +1)
  :config
  (evil-define-key 'insert comint-mode-map
    (kbd "<up>") 'comint-previous-input
    (kbd "<down>") 'comint-next-input)
  (evil-set-initial-state 'git-rebase-mode 'emacs)
  (evil-set-initial-state 'magit-popup-mode 'emacs)
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

;;; Ivy
(use-package ivy
  :ensure t
  :config (ivy-mode 1)

  (use-package counsel-projectile
    :ensure t
    :init (counsel-projectile-off)))

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
  (setq ag-reuse-buffers 't)
  (setq ag-highlight-search t))

;;; Magit
(use-package magit
  :ensure t
  :init
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

;;; rainbow-mode
(use-package rainbow-mode
  :ensure t)

;;; web-mode
(use-package web-mode
  :ensure t
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-content-types-alist
		  '(("jsx" . "\\.js[x]?\\'")))
  (add-to-list 'auto-mode-alist '("\\.htm[l]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
  :init
  (add-hook 'web-mode-hook
            (function (lambda()
                        (setq evil-shift-width web-mode-markup-indent-offset)))))

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

;;; TeX
(use-package tex
  :ensure auctex)

;;; JavaScript
(add-hook 'js-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq js-indent-level tab-width)
            (setq evil-shift-width js-indent-level)))

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

;;; PHP
(use-package php-mode
  :ensure t)

;; (init-open-recentf)
(put 'downcase-region 'disabled nil)

;;; mode-line
(let ((modeline.el (expand-file-name "modeline.el" user-emacs-directory)))
    (load modeline.el))
