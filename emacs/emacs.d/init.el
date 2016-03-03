(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (base16-grayscale-light)))
 '(custom-safe-themes
   (quote
    ("3ce3f4a0ef348612c9cdc260f9280a8283909cd9b20c913dc36e251d6801f6a2" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . " %i %-12:c%?-12t% s")
     (timeline . "  % s")
     (todo . " %i %-12:c")
     (tags . " %i %-12:c")
     (search . " %i %-12:c"))))
 '(org-agenda-remove-tags t)
 '(org-agenda-skip-archived-trees nil)
 '(org-agenda-use-time-grid nil)
 '(org-startup-truncated nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(setq ring-bell-function #'ignore)
(setq auto-save-default nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Themes
 (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
 (load-theme 'base16-grayscale-light)

(scroll-bar-mode -1)
;; (menu-bar-mode -1)

;; line numbers
;; (global-linum-mode 1)
;; (add-hook 'org-mode-hook (lambda () (linum-mode 0)))

;; Recentf
(require 'recentf)
(setq recentf-max-saved-items 50
	 recentf-max-menu-items 15)
(recentf-mode t)
(add-to-list 'recentf-exclude "\\.emacs.d/")

;;Auto Fill Mode (wraps line automatically)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Bookmarks
(setq inhibit-splash-screen t)
(require 'bookmark)
(bookmark-bmenu-list)
(setq initial-buffer-choice
      (lambda () (list-bookmarks) (get-buffer "*Bookmark List*")))

 ;; Org-mode
 (setq org-log-done t)
 (define-key global-map "\C-ca" 'org-agenda)
 (setq org-agenda-files '("~/Dropbox/Org"))
 (setq org-directory '"~/Dropbox/Org")
 (setq org-default-notes-file (concat org-directory "/Notes.org"))
 (setq org-refile-use-outline-path 'file)
 (setq org-refile-targets
	        '((org-agenda-files . (:maxlevel . 4))))
 (define-key global-map "\C-cc" 'org-capture)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "IDEA(i)" "STARTED(s@/!)" "WAITING(w@/!)" "|" "DONE(d)")
              (sequence "|" "CANCELLED(c@/!)" "DELEGATED(l@/!)"))))

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-agenda-custom-commands
      '(("W" "Completed and/or deferred tasks from previous week"
       ((agenda "" ((org-agenda-span 7)
            (org-agenda-start-day "-7d")
            (org-agenda-entry-types '(:timestamp))
			(org-agenda-prefix-format (quote ((agenda . " %i %-12:c") (timeline . "  % s") (todo . " %i %-12:c") (tags . " %i %-12:c") (search . " %i %-12:c"))))
            (org-agenda-show-log t)))))))

;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; use-package
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)

;;; Evil
(use-package evil
	:ensure t
	:config
	(evil-mode 1))

;;; Magit
(use-package magit
  :ensure t
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

;; (init-open-recentf)
