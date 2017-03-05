;; DOOM Molokai (inspired by molokai)

(require 'doom-themes)

(deftheme doom-molokai
  "A dark theme inspired by molokai")

(let ((c '((class color) (min-colors 89)))
      (bold   doom-enable-bold)
      (italic doom-enable-italic)

      (bg             "#1D1F20")
      (bg-l           "#222425")
      (fg             "#D6D6D4")
      (subtle         "#aab6c7")
      (vsubtle        "#556172")

      (black          "#000000")
      (grey           "#C0C5CF")
      (grey-.5        "#828284")
      (grey-1         "#525254")
      (grey-2         "#39393D")
      (white          "#FFFFFF")
      (yellow         "#E2C770")
      (orange         "#FD971F")
      (red            "#E74C3C")
      (magenta        "#F92672")
      (violet         "#9C91E4")
      (blue           "#268BD2")
      (blue+2         "#727280")
      (cyan           "#66D9EF")
      (green          "#B6E63E")
      (green-3        "#86B20E")
      (dark-cyan      "#8FA1B3"))

  (let* ((search-bg      green)
         (search-fg      black)
         (search-rest-bg violet)
         (search-rest-fg black)
         (highlight      orange)
         (vertical-bar   grey-2)
         (current-line   "#1F1F1F")
         (selection      "#535556")
         (builtin        orange)
         (comments       grey-1)
         (constants      green)
         (delimiters     "#c0c5ce")
         (functions      cyan)
         (keywords       magenta)
         (methods        dark-cyan)
         (operators      violet)
         (type           cyan)
         (strings        green)
         (variables      orange)

         (error-highlight red)

         (linum-bg       current-line)
         (linum-fg       "#3F3F48")
         (linum-hl-fg    orange)
         (linum-hl-bg    current-line)

         (active-minibuffer "#404046")
         (modeline-fg    white)
         (modeline-fg-2  orange)
         (modeline-fg-3  orange)
         (modeline-fg-inactive  "#80858F")
         (modeline-bg    grey-2)
         (modeline-bg-2  grey-2)
         (modeline-bg-3  grey-2)
         (modeline-bg-inactive  current-line)

         (vc-modified    grey-2)
         (vc-added       green-3)
         (vc-deleted     red))

    (custom-theme-set-faces
     'doom-molokai
     ;; Doom faces
     `(doom-default
       ((((type graphic)) :inherit default :background ,bg-l)
        (t                :inherit default)))
     `(doom-hl-line
       ((((type graphic)) :background ,bg)
        (t                :inherit hl-line)))
     `(doom-linum
       ((((type graphic)) :inherit linum :background ,bg-l)
        (t                :inherit linum)))
     `(doom-minibuffer-active ((,c (:background ,bg-l))))
     `(doom-nlinum-highlight  ((,c (:foreground ,linum-hl-fg :bold nil))))
     `(doom-flycheck-error    ((,c (:underline nil :foreground ,black :background ,red))))
     `(doom-flycheck-warning  ((,c (:underline nil :foreground ,black :background ,yellow))))
     `(doom-flycheck-info     ((,c (:underline nil :foreground ,black :background ,green))))
     ;; Text
     `(default                             ((,c (:foreground ,fg :background ,bg))))
     `(fringe                              ((,c (:background ,bg-l :foreground ,grey-1))))
     `(cursor                              ((,c (:background ,orange))))
     `(hl-line                             ((,c (:background ,bg-l))))
     `(region                              ((,c (:background ,grey-2 :foreground ,white))))
     `(highlight                           ((,c (:foreground ,yellow :inverse-video t))))
     `(shadow                              ((,c (:foreground ,orange))))
     `(minibuffer-prompt                   ((,c (:foreground ,orange))))
     `(tooltip                             ((,c (:background ,grey-2 :foreground ,orange))))
     `(error                               ((,c (:foreground ,red   ))))
     `(warning                             ((,c (:foreground ,yellow))))
     `(success                             ((,c (:foreground ,green ))))
     ;; `(secondary-selection              ((,c (:background ,orange))))
     ;; `(lazy-highlight                   ((,c (:background ,orange))))
     ;; `(match                            ((,c (:background ,magenta))))
     `(bold                                ((,c (:weight bold  :foreground ,white))))
     `(italic                              ((,c (:slant italic :foreground ,subtle))))
     `(bold-italic                         ((,c (:weight bold  :slant italic :foreground ,white))))
     `(trailing-whitespace                 ((,c (:background "#884444"))))
     `(whitespace-tab                      ((,c (:foreground ,grey-2))))
     `(whitespace-newline                  ((,c (:foreground ,grey-2))))
     `(whitespace-trailing                 ((,c (:background ,grey-2))))
     `(vertical-border                     ((,c (:foreground ,vertical-bar :background ,vertical-bar))))
     `(linum                               ((,c (:foreground ,linum-fg :background ,bg :bold nil))))
     `(font-lock-builtin-face              ((,c (:foreground ,builtin))))
     `(font-lock-comment-face              ((,c (:foreground ,comments))))
     `(font-lock-comment-delimiter-face    ((,c (:foreground ,comments))))
     `(font-lock-doc-face                  ((,c (:foreground ,blue+2))))
     `(font-lock-doc-string-face           ((,c (:foreground ,blue+2))))
     `(font-lock-constant-face             ((,c (:foreground ,constants))))
     `(font-lock-function-name-face        ((,c (:foreground ,functions))))
     `(font-lock-keyword-face              ((,c (:foreground ,keywords))))
     `(font-lock-string-face               ((,c (:foreground ,strings))))
     `(font-lock-type-face                 ((,c (:foreground ,type))))
     `(font-lock-variable-name-face        ((,c (:foreground ,variables))))
     `(font-lock-warning-face              ((,c (:foreground ,red))))
     `(font-lock-negation-char-face        ((,c (:foreground ,operators :bold t))))
     `(font-lock-preprocessor-char-face    ((,c (:foreground ,operators :bold t))))
     `(font-lock-regexp-grouping-backslash ((,c (:foreground ,operators :bold t))))
     `(font-lock-regexp-grouping-construct ((,c (:foreground ,operators :bold t))))
     `(show-paren-match                    ((,c (:foreground ,magenta :inverse-video t))))
     ;; Modeline
     `(mode-line                           ((,c (:foreground ,modeline-fg          :background ,modeline-bg))))
     `(mode-line-inactive                  ((,c (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))
     `(mode-line-is-modified               ((,c (:foreground ,magenta :background nil :bold t))))
     `(mode-line-buffer-file               ((,c (:foreground ,white :bold t))))
     `(mode-line-buffer-path               ((,c (:foreground ,grey))))
     `(mode-line-count-face                ((,c (:foreground ,black :background ,magenta))))
     `(spaceline-flycheck-error            ((,c (:underline nil :foreground ,black :background ,red))))
     `(spaceline-flycheck-warning          ((,c (:underline nil :foreground ,black :background ,yellow))))
     `(spaceline-flycheck-info             ((,c (:underline nil :foreground ,black :background ,green))))
     `(spaceline-highlight-face            ((,c (:foreground ,black :background ,highlight))))
     `(powerline-active1                   ((,c (:foreground ,modeline-fg-2 :background ,modeline-bg-2))))
     `(powerline-active2                   ((,c (:foreground ,modeline-fg-3 :background ,modeline-bg-3))))
     `(powerline-inactive1                 ((,c (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))
     `(powerline-inactive2                 ((,c (:foreground ,modeline-fg-inactive :background ,modeline-bg-inactive))))
     ;; Custom (doom)
     `(doom-modeline-buffer-path       ((,c (:foreground ,(if bold white yellow) :bold ,bold))))
     `(doom-modeline-buffer-project    ((,c (:foreground ,fg))))
     `(doom-modeline-buffer-modified   ((,c (:foreground ,orange))))
     `(doom-modeline-buffer-major-mode ((,c (:foreground ,white :bold ,bold))))

     `(doom-modeline-highlight     ((,c (:foreground ,orange))))
     `(doom-modeline-panel         ((,c (:foreground ,black :background ,orange))))
     `(doom-modeline-bar           ((,c (:background ,yellow))))
     `(doom-modeline-eldoc-bar     ((,c (:background ,red))))

     `(doom-modeline-info         ((,c (:foreground ,yellow))))
     `(doom-modeline-warning      ((,c (:background ,green))))
     `(doom-modeline-urgent       ((,c (:background ,red))))
     ;; Search
     `(isearch                             ((,c (:foreground ,search-fg :background ,search-bg))))
     `(isearch-lazy-highlight-face         ((,c (:foreground ,search-rest-fg :background ,search-rest-bg))))
     ;; `window-divider'
     `(window-divider              ((,c (:foreground ,vertical-bar))))
     `(window-divider-first-pixel  ((,c (:foreground ,vertical-bar))))
     `(window-divider-last-pixel   ((,c (:foreground ,vertical-bar))))

     ;;
     ;; Plugins
     ;;

     ;; Avy
     `(avy-lead-face-0    ((,c (:background ,orange :foreground ,black))))
     `(avy-lead-face-1    ((,c (:background ,orange :foreground ,black))))
     `(avy-lead-face-2    ((,c (:background ,orange :foreground ,black))))
     `(avy-lead-face      ((,c (:background ,orange :foreground ,black))))
     ;; company-mode
     `(company-tooltip             ((,c (:background ,black :foreground ,fg))))
     `(company-tooltip-common      ((,c (:foreground ,orange))))
     `(company-tooltip-search      ((,c (:foreground ,search-fg :background ,highlight))))
     `(company-tooltip-selection   ((,c (:background ,selection))))
     `(company-tooltip-mouse       ((,c (:background ,magenta :foreground ,bg))))
     `(company-scrollbar-bg        ((,c (:background ,black))))
     `(company-scrollbar-fg        ((,c (:background ,orange))))
     `(company-preview             ((,c (:foreground ,orange))))
     `(company-preview-common      ((,c (:foreground ,magenta :background ,grey-1))))
     `(company-preview-search      ((,c (:inherit company-tooltip-search))))
     ;; diff-hl
     `(diff-hl-change              ((,c (:foreground ,vc-modified))))
     `(diff-hl-delete              ((,c (:foreground ,vc-deleted))))
     `(diff-hl-insert              ((,c (:foreground ,vc-added))))
     ;; evil-mode
     `(evil-ex-substitute-replacement ((,c (:foreground ,magenta :background ,black :bold ,bold))))
     `(evil-search-highlight-persist-highlight-face ((,c (:background ,search-rest-bg))))
     ;; evil-snipe
     `(evil-snipe-first-match-face ((,c (:foreground ,search-fg :background ,search-bg))))
     `(evil-snipe-matches-face     ((,c (:foreground ,search-bg :underline t))))
     ;; flycheck
     `(flycheck-error              ((,c (:underline (:style wave :color ,red)    :background ,grey-2))))
     `(flycheck-warning            ((,c (:underline (:style wave :color ,yellow) :background ,grey-2))))
     `(flycheck-info               ((,c (:underline (:style wave :color ,green)  :background ,grey-2))))
     `(flyspell-incorrect          ((,c (:underline (:style wave :color ,error-highlight) :inherit unspecified))))
     ;; jabber
     `(jabber-activity-face ((,c (:inherit bold :foreground ,red))))
     `(jabber-activity-personal-face ((,c (:inherit bold :foreground ,blue))))
     `(jabber-chat-error ((,c (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-foreign ((,c (:inherit bold :foreground ,red))))
     `(jabber-chat-prompt-local ((,c (:inherit bold :foreground ,blue))))
     `(jabber-chat-prompt-system ((,c (:inherit bold :foreground ,green))))
     `(jabber-chat-text-foreign ((,c (:foreground ,fg))))
     `(jabber-chat-text-local ((,c (:foreground ,fg))))
     `(jabber-rare-time-face ((,c (:foreground ,green))))
     `(jabber-roster-user-away ((,c (:foreground ,yellow))))
     `(jabber-roster-user-chatty ((,c (:inherit bold :foreground ,green))))
     `(jabber-roster-user-dnd ((,c (:foreground ,red))))
     `(jabber-roster-user-error ((,c (:foreground ,red))))
     `(jabber-roster-user-offline ((,c (:foreground ,fg))))
     `(jabber-roster-user-online ((,c (:inherit bold :foreground ,green))))
     `(jabber-roster-user-xa ((,c (:foreground ,cyan))))
     ;; git-gutter
     `(git-gutter:modified         ((,c (:foreground ,vc-modified))))
     `(git-gutter:added            ((,c (:foreground ,vc-added))))
     `(git-gutter:deleted          ((,c (:foreground ,vc-deleted))))
     `(git-gutter-fr:modified      ((,c (:foreground ,vc-modified))))
     `(git-gutter-fr:added         ((,c (:foreground ,vc-added))))
     `(git-gutter-fr:deleted       ((,c (:foreground ,vc-deleted))))
     `(git-gutter+-modified        ((,c (:foreground ,vc-modified :background nil))))
     `(git-gutter+-added           ((,c (:foreground ,vc-added :background nil))))
     `(git-gutter+-deleted         ((,c (:foreground ,vc-deleted :background nil))))
     ;; Helm
     `(helm-selection              ((,c (:background ,selection))))
     `(helm-match                  ((,c (:foreground ,magenta))))
     `(helm-source-header          ((,c (:background ,current-line :foreground ,grey-1))))
     `(helm-swoop-target-line-face ((,c (:foreground ,highlight :inverse-video t))))
     `(helm-ff-file              ((,c (:foreground ,grey))))
     `(helm-ff-prefix            ((,c (:foreground ,magenta))))
     `(helm-ff-dotted-directory  ((,c (:foreground ,grey-1))))
     `(helm-ff-directory         ((,c (:foreground ,orange))))
     `(helm-ff-executable        ((,c (:foreground ,white :slant italic))))
     ;; hide-show
     `(hs-face                     ((,c (:foreground ,comments :background ,black))))
     `(hs-fringe-face              ((,c (:foreground ,orange))))
     ;; highlight-{quoted,numbers,indentation}-mode
     `(highlight-indentation-face                 ((,c (:background ,current-line))))
     `(highlight-indentation-current-column-face  ((,c (:background ,current-line))))
     `(highlight-quoted-symbol     ((,c (:foreground ,yellow))))
     `(highlight-quoted-quote      ((,c (:foreground ,magenta))))
     `(highlight-numbers-number    ((,c (:foreground ,constants))))
     ;; indent-guide,
     `(indent-guide-face           ((,c (:foreground "#2F2F38"))))
     ;; ivy
     `(ivy-current-match           ((,c (:background ,grey-2))))
     `(ivy-minibuffer-match-face-1 ((,c (:foreground ,yellow))))
     `(ivy-minibuffer-match-face-2 ((,c (:background ,black :foreground ,red :bold ,bold))))
     `(ivy-minibuffer-match-face-3 ((,c (:background ,black :foreground ,red :bold ,bold))))
     `(ivy-minibuffer-match-face-4 ((,c (:background ,black :foreground ,red :bold ,bold))))
     `(ivy-virtual                 ((,c (:foreground ,fg))))
     ;; neotree
     `(neo-root-dir-face           ((,c (:foreground ,green))))
     `(neo-file-link-face          ((,c (:foreground ,white))))
     `(neo-dir-link-face           ((,c (:foreground ,cyan))))
     `(neo-expand-btn-face         ((,c (:foreground ,magenta))))
     ;; pop-tip
     `(popup                       ((,c (:inherit tooltip))))
     `(popup-tip-face              ((,c (:inherit tooltip))))
     ;; rainbow-delimiters
     `(rainbow-delimiters-depth-1-face   ((,c (:foreground ,magenta))))
     `(rainbow-delimiters-depth-2-face   ((,c (:foreground ,orange))))
     `(rainbow-delimiters-depth-3-face   ((,c (:foreground ,yellow))))
     `(rainbow-delimiters-depth-4-face   ((,c (:foreground ,green))))
     `(rainbow-delimiters-depth-5-face   ((,c (:foreground ,cyan))))
     `(rainbow-delimiters-unmatched-face ((,c (:foreground ,red :inverse-video t))))
     ;; re-builder
     `(reb-match-0                 ((,c (:foreground ,orange   :inverse-video t))))
     `(reb-match-1                 ((,c (:foreground ,magenta  :inverse-video t))))
     `(reb-match-2                 ((,c (:foreground ,green    :inverse-video t))))
     `(reb-match-3                 ((,c (:foreground ,yellow   :inverse-video t))))
     ;; swiper
     `(swiper-line-face            ((,c (:background ,blue    :foreground ,black))))
     `(swiper-match-face-1         ((,c (:background ,black   :foreground ,grey))))
     `(swiper-match-face-2         ((,c (:background ,orange  :foreground ,black :bold ,bold))))
     `(swiper-match-face-3         ((,c (:background ,magenta :foreground ,black :bold ,bold))))
     `(swiper-match-face-4         ((,c (:background ,green   :foreground ,black :bold ,bold))))
     ;; stripe-buffer
     `(stripe-highlight            ((,c (:inherit doom-default))))
     ;; volatile highlights
     `(vhl/default-face            ((,c (:background ,grey-2))))
     ;; workgroups2
     `(wg-current-workgroup-face   ((,c (:foreground ,black   :background ,orange))))
     `(wg-other-workgroup-face     ((,c (:foreground ,grey-.5 :background ,current-line))))

     ;;
     ;; Language-specific
     ;;

     ;; (css|scss)-mode
     `(css-proprietary-property ((,c (:foreground ,keywords))))
     ;; js2-mode
     `(js2-function-param  ((,c (:foreground ,variables))))
     `(js2-function-call   ((,c (:foreground ,functions))))
     `(js2-object-property ((,c (:foreground ,methods))))
     `(js2-jsdoc-tag       ((,c (:foreground ,comments))))
     ;; web-mode
     `(web-mode-doctype-face           ((,c (:foreground ,comments))))
     `(web-mode-html-tag-face          ((,c (:foreground ,methods))))
     `(web-mode-html-tag-bracket-face  ((,c (:foreground ,methods))))
     `(web-mode-html-attr-name-face    ((,c (:foreground ,type))))
     `(web-mode-html-entity-face       ((,c (:foreground ,cyan :italic t))))
     `(web-mode-block-control-face     ((,c (:foreground ,orange))))
     ;;`(web-mode-html-tag-bracket-face  ((,c (:foreground ,operators))))
     ;; markdown-mode
     `(markdown-header-face           ((,c (:foreground ,orange))))
     `(markdown-header-delimiter-face ((,c (:foreground ,orange))))
     `(markdown-blockquote-face       ((,c (:foreground ,blue+2))))
     `(markdown-markup-face           ((,c (:foreground ,cyan))))
     `(markdown-inline-face           ((,c (:foreground ,cyan))))
     `(markdown-list-face             ((,c (:foreground ,magenta))))
     `(markdown-pre-face              ((,c (:foreground ,cyan))))
     `(markdown-header-face-1         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-2         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-3         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-4         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-5         ((,c (:inherit markdown-header-face))))
     `(markdown-header-face-6         ((,c (:inherit markdown-header-face))))
     ;;`(markdown-header-rule-face       (:inherit shadow))
     ;;`(markdown-italic-face            (:inherit italic))
     ;;`(markdown-link-face              (:inherit shadow))
     ;;`(markdown-link-title-face        (:inherit link))
     ;;`(markdown-url-face               (:inherit link))
     ;; org-mode
     `(org-tag                      ((,c (:foreground ,yellow :bold nil))))
     ;;`(org-ellipsis               ((,c (:inherit hs-face))))
     `(org-hide                     ((,c (:foreground ,bg))))
     `(org-table                    ((,c (:foreground ,cyan))))
     `(org-quote                    ((,c (:slant italic :foreground ,grey :background ,current-line))))
     `(org-document-info            ((,c (:foreground ,orange))))
     `(org-document-info-keyword    ((,c (:foreground ,grey-1))))
     `(org-meta-line                ((,c (:foreground ,vsubtle))))
     `(org-block-begin-line         ((,c (:background ,current-line :foreground ,vsubtle))))
     `(org-block-end-line           ((,c (:inherit org-block-begin-line))))
     `(org-block-background         ((,c (:background ,current-line))))
     `(org-archived                 ((,c (:foreground ,grey-.5))))
     `(org-document-title           ((,c (:foreground ,cyan))))
     `(org-level-1                  ((,c (:background ,current-line :foreground ,magenta   :bold ,bold))))
     `(org-level-2                  ((,c (                          :foreground ,dark-cyan :bold ,bold))))
     `(org-level-3                  ((,c (                          :foreground ,violet    :bold ,bold))))
     `(org-level-4                  ((,c (                          :foreground ,green     :bold ,bold))))
     `(org-level-5                  ((,c (                          :foreground ,yellow))))
     `(org-level-6                  ((,c (                          :foreground ,blue+2))))
     `(org-code                     ((,c (:foreground ,orange))))
     `(org-verbatim                 ((,c (:foreground ,green))))
     `(org-formula                  ((,c (:foreground ,cyan))))
     `(org-list-dt                  ((,c (:foreground ,cyan))))
     `(org-footnote                 ((,c (:foreground ,orange))))
     `(org-link                     ((,c (:underline t :foreground ,cyan :bold inherit))))
     `(org-date                     ((,c (:foreground ,violet))))
     `(org-todo                     ((,c (:foreground ,yellow :bold inherit))))
     `(org-done                     ((,c (:foreground ,green :bold inherit))))
     `(org-headline-done            ((,c (:foreground ,grey-.5 :strike-through t :bold nil))))
     `(org-special-keyword          ((,c (:foreground ,magenta))))
     `(org-checkbox-statistics-todo ((,c (:inherit org-todo))))
     `(org-checkbox-statistics-done ((,c (:inherit org-done))))
     ;;; rpm-spec-mode
     `(rpm-spec-macro-face        ((,c (:foreground ,yellow))))
     `(rpm-spec-var-face          ((,c (:foreground ,violet))))
     `(rpm-spec-tag-face          ((,c (:foreground ,blue))))
     `(rpm-spec-obsolete-tag-face ((,c (:foreground ,red))))
     `(rpm-spec-package-face      ((,c (:foreground ,orange))))
     `(rpm-spec-dir-face          ((,c (:foreground ,green))))
     `(rpm-spec-doc-face          ((,c (:foreground ,orange))))
     `(rpm-spec-ghost-face        ((,c (:foreground ,(doom-lighten grey-1 0.2)))))
     `(rpm-spec-section-face      ((,c (:foreground ,magenta))))
     )

    (custom-theme-set-variables
     'doom-molokai
     `(vc-annotate-color-map
       '((20 .  ,green)
         (40 .  ,(doom-blend yellow green (/ 1.0 3)))
         (60 .  ,(doom-blend yellow green (/ 2.0 3)))
         (80 .  ,yellow)
         (100 . ,(doom-blend orange yellow (/ 1.0 3)))
         (120 . ,(doom-blend orange yellow (/ 2.0 3)))
         (140 . ,orange)
         (160 . ,(doom-blend magenta orange (/ 1.0 3)))
         (180 . ,(doom-blend magenta orange (/ 2.0 3)))
         (200 . ,magenta)
         (220 . ,(doom-blend red magenta (/ 1.0 3)))
         (240 . ,(doom-blend red magenta (/ 2.0 3)))
         (260 . ,red)
         (280 . ,(doom-blend grey red (/ 1.0 4)))
         (300 . ,(doom-blend grey red (/ 2.0 4)))
         (320 . ,(doom-blend grey red (/ 3.0 4)))
         (340 . ,grey)
         (360 . ,grey)))
     `(vc-annotate-very-old-color nil)
     `(vc-annotate-background ,black))))

(provide-theme 'doom-molokai)

;; Local Variables:
;; no-byte-compile: t
;; End:
