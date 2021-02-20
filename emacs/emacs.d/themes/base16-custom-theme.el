(require 'base16-3024-theme)
(require 'base16-grayscale-light-theme)

(deftheme base16-custom)

(let ((base-gray-00 (plist-get base16-grayscale-light-colors :base00)) ;; #f7f7f7
      (base-gray-01 (plist-get base16-grayscale-light-colors :base01)) ;; #e3e3e3
      (base-gray-02 (plist-get base16-grayscale-light-colors :base02)) ;; #b9b9b9
      (base-gray-03 (plist-get base16-grayscale-light-colors :base03)) ;; #ababab
      (base-gray-04 (plist-get base16-grayscale-light-colors :base04)) ;; #525252
      (base-gray-05 (plist-get base16-grayscale-light-colors :base05)) ;; #464646
      (base-gray-06 (plist-get base16-grayscale-light-colors :base06)) ;; #252525
      (base-gray-07 (plist-get base16-grayscale-light-colors :base07)) ;; #101010
      (base-gray-08 (plist-get base16-grayscale-light-colors :base08)) ;; #7c7c7c
      (base-gray-0A (plist-get base16-grayscale-light-colors :base0A)) ;; #a0a0a0
      (base-gray-0B (plist-get base16-grayscale-light-colors :base0B)) ;; #8e8e8e
      (base-gray-0C (plist-get base16-grayscale-light-colors :base0C)) ;; #868686
      (base-gray-0D (plist-get base16-grayscale-light-colors :base0D)) ;; #686868
      (base-gray-0E (plist-get base16-grayscale-light-colors :base0E)) ;; #747474
      (base-gray-0F (plist-get base16-grayscale-light-colors :base0F)) ;; #5e5e5e
      (base-3024-08 (plist-get base16-3024-colors            :base08)) ;; #db2d20
      (base-3024-09 (plist-get base16-3024-colors            :base09)) ;; #e8bbd0
      (base-3024-0A (plist-get base16-3024-colors            :base0A)) ;; #fded02
      (base-3024-0B (plist-get base16-3024-colors            :base0B)) ;; #01a252
      (base-3024-0C (plist-get base16-3024-colors            :base0C)) ;; #b5e4f4
      (base-3024-0D (plist-get base16-3024-colors            :base0D)) ;; #01a0e4
      (base-3024-0E (plist-get base16-3024-colors            :base0E)) ;; #a16a94
      (base-3024-0F (plist-get base16-3024-colors            :base0F)) ;; #cdab53
      (base-custom-diff-change "#DDEEFF")
      (base-custom-cursor "#AB4642"))

  (custom-theme-set-faces
   'base16-custom

   ;; ag-mode
   `(ag-match-face ((t (:inherit highlight :weight bold :background nil))))

   ;; basic colors
   `(cursor ((t (:background ,base-custom-cursor))))
   `(fringe ((t (:background ,base-gray-00))))
   `(highlight ((t (:foreground ,base-custom-cursor))))
   `(link ((t (:underline t :foreground ,base-gray-0D))))
   `(link-visited ((t (:underline t :foreground ,base-gray-0E))))
   `(region ((t (:background ,base-3024-0A))))
   `(secondary-selection ((t (:background ,base-gray-01))))
   `(vertical-border ((t (:foreground ,base-gray-03))))
   `(widget-field ((t (:foreground ,base-gray-07 :background ,base-gray-04 :box (:line-width 1 :color ,base-gray-01)))))

   ;; column-marker
   `(column-marker-1 ((t (:background nil :inherit highlight))))

   ;; compilation
   `(compilation-column-number ((t (:inherit default :underline t))))
   `(compilation-info ((t (:weight bold :foreground ,base-gray-08))))
   `(compilation-line-number ((t (:inherit default :underline t))))

   ;; company-mode
   `(company-tooltip ((t (:background ,base-gray-05 :foreground ,base-gray-00))))
   `(company-tooltip-selection ((t (:background ,base-3024-0A :foreground ,base-gray-07))))
   `(company-tooltip-search ((t (:foreground ,base-custom-cursor))))
   `(company-tooltip-common ((t (:weight bold))))
   `(company-tooltip-annotation ((t (:foreground ,base-gray-01))))
   `(company-tooltip-annotation-selection ((t (:foreground ,base-gray-04))))
   `(company-scrollbar-fg ((t (:background ,base-custom-cursor))))
   `(company-scrollbar-bg ((t (:background ,base-gray-08))))
   `(company-preview ((t (:background ,base-3024-0A))))

   ;; diff-hl-mode
   `(diff-hl-change ((t (:background ,base-custom-diff-change))))
   `(diff-hl-delete ((t (:inherit magit-diff-removed))))
   `(diff-hl-insert ((t (:inherit magit-diff-added))))
   `(diff-hl-unknown ((t (:foreground ,base-gray-02))))

   ;; diff-hl-mode
   `(diff-added ((t (:foreground ,base-3024-0B))))
   `(diff-changed ((t (:foreground ,base-3024-0E))))
   `(diff-file-header ((t (:foreground ,base-gray-02))))
   `(diff-header ((t (:foreground ,base-gray-02))))
   `(diff-hunk-header ((t (:foreground ,base-gray-02))))
   `(diff-removed ((t (:foreground ,base-custom-cursor))))

   ;; font-lock
   `(font-lock-constant-face ((t (:foreground ,base-3024-0D))))
   `(font-lock-warning-face ((t (:foreground ,base-custom-cursor :weight bold))))

   ;; elixir-mode
   `(elixir-atom-face ((t (:foreground ,base-3024-0D))))
   `(elixir-attribute-face ((t (:foreground ,base-3024-0E))))

   ;; epresent
   `(epresent-author-face ((t (:height 360))))
   `(epresent-heading-face ((t (:height 360))))
   `(epresent-subheading-face ((t (:height 360))))

   ;; gscholar
   `(gscholar-bibtex-title ((t (:height 1.0 :foreground ,base-3024-0B))))

   ;; hlinum-mode
   `(linum-highlight-face ((t (:background ,base-gray-00 :foreground ,base-3024-09))))

   ;; isearch
   `(isearch ((t (:foreground ,base-3024-0A :background ,base-gray-06 :inverse-video t))))
   `(isearch-lazy-highlight-face ((t (:foreground ,base-gray-02 :background ,base-gray-06 :inverse-video t))))

   ;; linum-mode
   `(linum ((t (:background ,base-gray-00 :foreground ,base-gray-01))))

   ;; magit
   `(magit-branch-current ((t (:foreground ,base-3024-0D :weight bold))))
   `(magit-branch-local ((t (:foreground ,base-3024-0D))))
   `(magit-branch-remote ((t (:foreground ,base-3024-0B))))
   `(magit-diff-hunk-heading ((t (:background ,base-gray-01))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,base-gray-02))))
   `(magit-popup-argument ((t (:inherit highlight :weight bold :background nil))))
   `(magit-popup-argument ((t (:inherit highlight :weight bold :background nil))))
   `(magit-popup-key ((t (:foreground ,base-3024-0D))))
   `(magit-section-heading ((t (:foreground ,base-3024-0F :weight bold))))

   ;; mode-line
   `(mode-line ((t (:background ,base-3024-0A :foreground ,base-gray-07 :box ,base-gray-04))))
   `(mode-line-inactive ((t (:background ,base-gray-00 :foreground ,base-gray-03 :box ,base-gray-03))))

   ;; org-mode
   `(org-date ((t (:foreground ,base-gray-0E))))
   `(org-drawer ((t (:foreground ,base-gray-0E))))
   `(org-done ((t (:background ,base-gray-00 :foreground ,base-gray-02))))
   `(org-headline-done ((t (:background ,base-gray-00 :foreground ,base-gray-02))))
   `(org-todo ((t (:background ,base-gray-00))))
   `(org-hide ((t (:foreground ,base-gray-06))))
   `(org-level-1 ((t (:foreground ,base-3024-0D :weight bold))))
   `(org-level-2 ((t (:foreground ,base-gray-05 :weight bold))))
   `(org-level-3 ((t (:foreground ,base-3024-0E))))
   `(org-link ((t (:underline t :foreground ,base-gray-0D))))
   `(org-link ((t (:underline t :foreground ,base-gray-0D))))
   `(org-priority ((t (:foreground ,base-3024-0D))))
   `(org-scheduled-today ((t (:foreground ,base-3024-0B :weight bold))))
   `(org-tag ((t (:foreground ,base-gray-07 :weight normal))))

   ;; show-paren-mode
   `(show-paren-match ((t (:foreground ,base-3024-08 :background ,base-gray-00 :weight bold))))

   ))

(provide-theme 'base16-custom)
