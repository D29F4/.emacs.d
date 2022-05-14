; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  theme-base.el
;;
;;  Theme base.
;;
;;===========================================================================



;============================================================================
;  construct-faces (adjutant function)
;----------------------------------------------------------------------------
(defun custom-theme-construct-faces (display faces)
  "Construct the expected structure for face definitions given
DISPLAY and a list of FACES (that is: a mapping of properties to
face names).

See the documentation for defface for information about the
expected structure.

(As noted: this accepts a list of faces.  That permits convenient
interpolation of multiple prepared face definitions.  The
argument list would be (display &rest faces) if a flat array of
faces is instead to be expected.)"
  (let (structure)
    (dolist (face faces structure)
      (push
        `(
          ;  The element name
          ,(car face)
          ;  The SPEC element of defface (consisting of the display value
          ;  and the face attributes)
          ((,display ,(cdr face)))
        )
        structure))))
;____________________________________________________________________________



;============================================================================
;  Customization Groups
;----------------------------------------------------------------------------
(defgroup comparison-faces ()
  "Faces representing comparisons suggesting additions to,
deletions of, or consistency of matter.")

(defgroup display-faces ()
  "Faces of structural components of textual and functional
displays within documents or interactive buffer environments.")

(defgroup selection-faces ()
  "Faces representing matter selected in some fashion.")

(defgroup status-faces ()
  "Faces representing various statuses or conditions of matter,
actions, et cetera.")

(defgroup structural-faces ()
  "Faces of infrastructural components of buffers.")
;____________________________________________________________________________



;============================================================================
;  New Faces
;
;  It seems that when some elements (at least buttons and links) are defined
;  later (that is: in custom-theme-default-color-map) the specified foreground
;  and background colors are not applied.  font-class faces are therefore
;  present in order to apply such styling.
;
;  The above apparently does not apply to the "default" face as employing a
;  font-class-default face applies only some of the defined traits.
;
;
;  font-lock.el currently states that "in new code, in the vast majority of
;  cases there is no need to create variables that specify face names. [...]
;  Font-lock is not a template to be followed in this area."  Apparently
;  quoting the defined face name is satisfactory.  If an alternative is
;  necessary then we may explicitly define variables for new faces thusly:
;    (defvar font-class-x 'font-class-x
;      "Description.")
;
;----------------------------------------------------------------------------
;  General
;----------------------------------------------------------------------------
(defface font-class-highlight
  nil
  "Font face for matter to be particularly distinguished, either
as a generic signal of modification or dynamically-applied
distinction or as notable matter in a heterogeneous context."
  :group 'font-lock-faces)

(defface font-class-signal
  nil
  "Font face for matter to be generically distinguished, possibly
in a primarily 'default' context.

As such it may be typical to function as a brighter version of
the default face.

It is not, however, necessarily expected to be or to provide the
value for the internal 'highlight' face (which is more properly
served by font-class-highlight)."
  :group 'font-lock-faces)

(defface font-class-muted
  nil
  "Font face for matter intended to lack prominence."
  :group 'font-lock-faces)

;----------------------------------------------------------------------------
;  Status
;----------------------------------------------------------------------------
;  Warning (background)
(defface font-class-warning-bg
  nil
  "Background-focused styling for matter related to a warning state."
  :group 'font-lock-faces
  :group 'status-faces)

;  Error
(defface font-class-error
  nil
  "Font face for matter related to an error or failure state."
  :group 'font-lock-faces
  :group 'status-faces)

;  Success
;  (Possibly of fairly limited applicability.)
(defface font-class-success
  nil
  "Font face for matter related to a successful state."
  :group 'font-lock-faces
  :group 'status-faces)

;----------------------------------------------------------------------------
;  Structural Components of Buffers
;----------------------------------------------------------------------------
(defface font-class-mode-line
  nil
  "Face for the active mode line."
  :group 'structural-faces
  :group 'mode-line-faces)

(defface font-class-mode-line-inactive
  nil
  "Face for an inactive mode line."
  :group 'structural-faces
  :group 'mode-line-faces)

(defface font-class-peripheral
  nil
  "Face for the fringe and any similar peripheral spaces."
  :group 'structural-faces)

(defface font-class-peripheral-highlight
  nil
  "Face for distinctive matter in peripheral spaces."
  :group 'structural-faces)

(defface font-class-button
  nil
  "Face for button devices."
  :group 'widget-faces)

;----------------------------------------------------------------------------
;  Structural Components of Displays
;----------------------------------------------------------------------------
(defface font-class-heading
  nil
  "Master face for headings."
  :group 'font-lock-faces
  :group 'display-faces)

(defface font-class-heading-segment
  nil
  "Face for headings of matter segments which are likely to
themselves be variously styled.

Intended to provide a satisfactory level of distinction to (for
example) subsections of differentials (called by many 'hunks')
in diff-type results."
  :group 'font-lock-faces
  :group 'display-faces)

(defface font-class-link
  nil
  "Face for links."
  :group 'font-lock-faces
  :group 'display-faces)

(defface font-class-link-visited
  nil
  "Face for visited links."
  :group 'font-lock-faces
  :group 'display-faces)

;----------------------------------------------------------------------------
;  Selection, Search
;----------------------------------------------------------------------------
(defface font-class-select-primary
  nil
  "Face for highlighting (primary) selection or search
results or related matter."
  :group 'font-lock-faces
  :group 'selection-faces)

(defface font-class-select-secondary
  nil
  "Face for highlighting (secondary) selection or search
results or related matter."
  :group 'font-lock-faces
  :group 'selection-faces)

(defface font-class-select-region
  nil
  "Face for highlighting regionally-selected matter."
  :group 'font-lock-faces
  :group 'selection-faces)

(defface font-class-select-region-distinct
  nil
  "Face for distinctive highlighting (as (for example) in a
header context) within regionally-highlighted matter."
  :group 'font-lock-faces
  :group 'selection-faces)

;----------------------------------------------------------------------------
;  Code Elements
;----------------------------------------------------------------------------
(defface font-class-declaration
  nil
  "Face for declarative, labelling, phase-level, or sub-functional keywords."
  :group 'font-lock-faces)

;----------------------------------------------------------------------------
;  Comparison Elements
;----------------------------------------------------------------------------
(defface font-class-added
  nil
  "Face for added matter in comparison results."
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-added-reverse
  nil
  "Blocking or highlighting of added matter.  (Expected to define
background styling.)"
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-added-reverse-hl
  '((t (:inherit font-class-added-reverse
        :inverse-video t)))
  "Distinctive styling within -reversed added matter."
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-changed
  nil
  "Face for changed matter in comparison results."
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-changed-reverse
  nil
  "Blocking or highlighting of changed matter.  (Expected to
define background styling.)"
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-changed-reverse-hl
  '((t (:inherit font-class-changed-reverse
        :inverse-video t)))
  "Distinctive styling within -reversed changed matter."
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-removed
  nil
  "Face for removed matter in comparison results."
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-removed-reverse
  nil
  "Blocking or highlighting of removed matter.  (Expected to
define background styling.)"
  :group 'font-lock-faces
  :group 'comparison-faces)

(defface font-class-removed-reverse-hl
  '((t (:inherit font-class-removed-reverse
        :inverse-video t)))
  "Distinctive styling within -reversed removed matter."
  :group 'font-lock-faces
  :group 'comparison-faces)
;____________________________________________________________________________



;============================================================================
;  Face Inheritance
;----------------------------------------------------------------------------
(defun custom-theme-default-init-map (theme_identifier)
  "Define faces (primarily through inheritance), construct the
appropriate forms, and set using custom-theme-set-faces."

  (let ((faces (list

    ;------------------------------------------------------------------------
    ;  1  Common elements
    ;
    ;  Comments:
    ;    . highlight: includes pointer selection of dired items.
    ;    . lazy-highlight: secondary search highlighting.
    ;    . shadow: used by ~ files in dired.
    ;------------------------------------------------------------------------
    `(default                          :family  "DejaVu Serif Mono"
                                       :foundry "foundry"
                                       :width   normal
                                       :height  75
                                       :weight  normal
                                       :slant   normal
                                       :underline nil
                                       :overline  nil
                                       :strike-through nil
                                       :box     nil
                                       :inverse-video nil
                                       :distant-foreground nil
                                       :stipple nil
                                       :inherit nil)
    `(error                            :inherit font-class-error)
    `(escape-glyph                     :inherit font-lock-warning-face)
    ;`(fixed-pitch                     :family  "Monospace")
    `(fixed-pitch-serif                :inherit fixed-pitch)
    `(fringe                           :inherit font-class-peripheral)
    `(highlight                        :inherit font-class-highlight)
    `(homoglyph                        :inherit font-lock-warning-face)
    `(isearch                          :inherit font-class-select-primary)
    `(isearch-fail                     :inherit font-class-error)
    `(lazy-highlight                   :inherit font-class-select-secondary)
    `(line-number                      :inherit font-class-peripheral
                                       :height  65)
    `(line-number-current-line         :inherit font-class-peripheral-highlight
                                       :height  65)
    `(link                             :inherit font-class-link)
    `(link-visited                     :inherit font-class-link-visited)
    `(match                            :inherit font-class-select-secondary)
    `(minibuffer-prompt                :inherit default)
    `(mode-line                        :inherit font-class-mode-line)
    `(mode-line-highlight              :inherit font-class-highlight)
    `(mode-line-inactive               :inherit font-class-mode-line-inactive)
    `(nobreak-hyphen                   :inherit font-lock-warning-face)
    `(nobreak-space                    :inherit font-lock-warning-face
                                       :underline t)
    `(outline-1                        :inherit default)
    `(outline-2                        :inherit default)
    `(outline-3                        :inherit default)
    `(outline-4                        :inherit default)
    `(outline-5                        :inherit default)
    `(outline-6                        :inherit default)
    `(outline-7                        :inherit default)
    ;`(region                          :inherit font-class-select-region)
    `(secondary-selection              :inherit font-class-select-secondary)
    `(shadow                           :inherit font-class-muted)
    `(show-paren-match                 :inherit font-class-highlight
                                       :bold    t)
    `(show-paren-mismatch              :inherit font-class-error
                                       :bold    t)
    ;`(show-paren-match-expression     :inherit font-class-highlight)
    `(success                          :inherit font-class-success)
    `(trailing-whitespace              :inherit font-lock-warning-face
                                       :strike-through t)
    `(variable-pitch                   :inherit fixed-pitch)
    `(warning                          :inherit font-lock-warning-face)
    `(which-func                       :inherit font-class-highlight)
    ;`(linum                           :height  65)
    ;`(scroll-bar                      nil)

    ;------------------------------------------------------------------------
    ;  2  Font classes
    ;------------------------------------------------------------------------
    `(font-lock-builtin-face           :inherit font-lock-constant-face)
    `(font-lock-comment-face           :inherit font-class-muted)
    `(font-lock-comment-delimiter-face :inherit font-lock-comment-face)
    `(font-lock-doc-face               :inherit font-lock-string-face)
    `(font-lock-negation-char-face     :inherit font-lock-keyword-face)
    `(font-lock-regexp-grouping-backslash :inherit font-class-signal)

    ;------------------------------------------------------------------------
    ;  3  Specialized domains
    ;------------------------------------------------------------------------
    ;  calendar
    `(calendar-month-header            :inherit font-class-heading)
    `(calendar-today                   :inherit font-class-signal)
    `(calendar-weekday-header          :inherit font-lock-type-face)
    `(calendar-weekend-header          :inherit font-lock-keyword-face)
    `(holiday                          :inherit font-lock-constant-face)

    ;  cperl-mode
    `(cperl-array-face                 :inherit font-lock-variable-name-face)
    `(cperl-hash-face                  :inherit font-lock-variable-name-face)

    ;  CSS
    `(css-property                     :inherit font-lock-variable-name-face)
    `(css-selector                     :inherit font-lock-function-name-face)

    ;  Diff
    `(diff-added                       :inherit font-class-added)
    `(diff-changed                     :inherit font-class-changed)
    `(diff-context                     :inherit default)
    `(diff-file-header                 :inherit font-class-heading)
    `(diff-header                      :inherit font-class-heading
                                       :weight  bold)
    `(diff-hunk-header                 :inherit font-class-heading-segment)
    `(diff-nonexistent                 :inherit (diff-file-header
                                                 font-class-removed))
    `(diff-refine-added                :inherit font-class-added-reverse-hl)
    `(diff-refine-changed              :inherit font-class-changed-reverse-hl)
    `(diff-refine-removed              :inherit font-class-removed-reverse-hl)
    `(diff-removed                     :inherit font-class-removed)

    ;  Dired
    `(dired-broken-symlink             :inherit font-lock-error-face)
    `(dired-directory                  :inherit font-class-signal)
    `(dired-flagged                    :inherit font-lock-warning-face)
    `(dired-header                     :inherit font-class-heading)
    `(dired-mark                       :inherit font-class-signal)
    `(dired-marked                     :inherit highlight)
    `(dired-perm-write                 :inherit font-class-signal)
    `(dired-symlink                    :inherit font-lock-string-face)

    ;  eww
    `(eww-form-checkbox                :inherit font-class-button)
    `(eww-form-file                    :inherit font-class-button)
    `(eww-form-select                  :inherit font-class-button)
    `(eww-form-submit                  :inherit font-class-button)
    `(eww-form-text                    :inherit widget-field)
    `(eww-form-textarea                :inherit widget-field)
    `(eww-invalid-certificate          :inherit font-class-error)
    `(eww-valid-certificate            :inherit font-class-success)

    ;  Git commit
    `(git-commit-comment-branch-local  :inherit font-class-signal
                                       :weight  bold)
    `(git-commit-comment-branch-remote :inherit git-commit-comment-branch-local
                                       :inverse-video t)
    `(git-commit-summary               :inherit font-class-signal)

    ;  git-gutter
    `(git-gutter:added                 :inherit font-class-added-reverse
                                       :foreground nil)
    `(git-gutter:deleted               :inherit font-class-removed-reverse
                                       :foreground nil)
    `(git-gutter:modified              :inherit font-class-changed-reverse
                                       :foreground nil)

    ;  Info
    `(info-title-4                     :inherit default
                                       :weight  bold)

    ;  LaTeX
    `(font-latex-warning-face          :inherit font-lock-warning-face)

    ;  Log
    `(log-edit-header                  :inherit font-lock-constant-face)

    ;  Magit
    `(magit-branch-local               :inherit font-class-signal)
    `(magit-branch-remote              :inherit magit-branch-local
                                       :weight  bold)
    `(magit-diff-added                 :inherit font-class-added)
    `(magit-diff-added-highlight       :inherit font-class-added-reverse-hl)
    `(magit-diff-context               :inherit default)
    `(magit-diff-file-heading          :inherit font-class-heading)
    `(magit-diff-hunk-heading          :inherit font-class-heading-segment)
    `(magit-diff-lines-heading         :inherit
                                          (list font-class-select-region-distinct
                                                magit-diff-hunk-heading
                                                magit-diff-hunk-heading-highlight))
    `(magit-diff-removed               :inherit font-class-removed)
    `(magit-diff-removed-highlight     :inherit font-class-removed-reverse)
    `(magit-diffstat-added             :inherit magit-diff-added)
    `(magit-diffstat-removed           :inherit magit-diff-removed)
    `(magit-dimmed                     :inherit font-class-muted)
    `(magit-hash                       :inherit font-lock-constant-face)
    `(magit-log-author                 :inherit font-lock-keyword-face)
    `(magit-log-date                   :inherit default)
    `(magit-process-ng                 :inherit font-class-error
                                       :weight  bold
                                       :foreground nil)
    `(magit-process-ok                 :inherit font-class-success
                                       :weight  bold
                                       :foreground nil)
    `(magit-section-heading            :inherit font-class-heading
                                       :weight bold)
    `(magit-tag                        :inherit font-lock-constant-face)

    ;  Markdown
    `(markdown-blockquote-face         :inherit default)
    `(markdown-code-face               :inherit font-lock-string-face)
    `(markdown-footnote-text-face      :inherit default)
    `(markdown-gfm-checkbox-face       :inherit link)
    `(markdown-header-delimiter-face   :inherit markdown-header-face)
    `(markdown-header-face             :bold    t
                                       :inherit font-class-heading)
    `(markdown-highlighting-face       :inherit font-class-select-primary)
    `(markdown-html-tag-delimiter-face :inherit markdown-html-tag-name-face)
    `(markdown-html-tag-name-face      :inherit font-lock-function-name-face)
    `(markdown-inline-code-face        :inherit font-lock-string-face)
    `(markdown-line-break-face         :underline t
                                       :inherit font-class-warning)
    `(markdown-link-title-face         :inherit default)
    `(markdown-list-face               :inherit default)
    `(markdown-pre-face                :inherit markdown-code-face)

    ;  raku-mode
    `(raku-declare                     :inherit font-lock-function-name-face)
    `(raku-label                       :inherit font-class-declaration)
    `(raku-number                      :inherit default)
    `(raku-operator                    :inherit default)
    `(raku-phaser                      :inherit font-class-declaration)
    `(raku-pragma                      :inherit font-lock-preprocessor-face)
    `(raku-twigil                      :inherit font-lock-variable-name-face)
    `(raku-type                        :inherit default)
    `(raku-type-constraint             :inherit default)

    ;  smerge-mode
    `(smerge-base                      :inherit default)
    `(smerge-lower                     :inherit font-class-added-reverse)
    `(smerge-markers                   :inherit default)
    `(smerge-refined-added             :inherit font-class-added-reverse-hl) ; + smerge-refined-change?
    `(smerge-refined-removed           :inherit font-class-removed-reverse-hl) ; + smerge-refined-change?
    `(smerge-upper                     :inherit font-class-removed-reverse)

    ;  TeX
    `(tex-math-face                    :inherit font-lock-string-face)

    ;  whitespace-mode
    `(whitespace-big-indent            :inherit font-class-warning-bg)
    `(whitespace-empty                 :inherit font-class-warning-bg)
    `(whitespace-hspace                :underline t
                                       :inherit font-lock-warning-face)
    `(whitespace-indentation           :inherit font-class-warning-bg)
    `(whitespace-line                  :inherit font-lock-warning-face)
    `(whitespace-newline               :inherit font-class-signal)
    `(whitespace-space                 :bold    t
                                       :inherit font-lock-warning-face)
    `(whitespace-space-after-tab       :inherit whitespace-tab)
    `(whitespace-space-before-tab      :inherit whitespace-tab)
    `(whitespace-tab                   :inherit highlight
                                       :inherit font-class-warning-bg)
    `(whitespace-trailing              :inherit font-class-warning-bg)

    ;------------------------------------------------------------------------
    ;  4  Emacs internal
    ;------------------------------------------------------------------------
    ;  The Customize buffer
    `(custom-button                    :inherit font-class-button)
    `(custom-button-mouse              :inherit custom-button)
    `(custom-button-pressed            :inherit custom-button
                                       :box     (:line-width 1
                                                 :style pressed-button))
    `(custom-group-tag                 :height  1.2
                                       :weight  normal
                                       :inherit font-class-heading)
    `(custom-group-tag-1               :weight  normal
                                       :inherit custom-group-tag)
    `(custom-state                     :inherit font-lock-doc-face)
    `(custom-variable-tag              :inherit font-class-heading)
    `(custom-visibility                :inherit link
                                       :height  0.9)

    ;  UI devices
    `(widget-single-line-field         :inherit widget-field)

  )))

  ;--------------------------------------------------------------------------
  ;  Engage custom-theme-set-faces
  ;--------------------------------------------------------------------------
  (apply
    'custom-theme-set-faces
    theme_identifier
    (custom-theme-construct-faces t faces)
  )
))
;____________________________________________________________________________



;============================================================================
;  The Default Color Map
;
;  Some faces are defined here with explicit color variables instead of more
;  properly receiving inheritance in custom-theme-default-init-map.
;  Apparently the inheritance at this stage is not recognized by some
;  functionality.
;
;  Namely:
;    . region: inherits as intended but the background value provided is not
;      recognized by kill-ring-save.
;    . magit-diff-context-highlight: does not inherit values as intended if
;      defined via the initialization map, where it should inherit
;      font-class-select-region.  Defining it here only.
;----------------------------------------------------------------------------
(defun custom-theme-default-color-map ()
  "Return face definitions as a list.  Color variables (c-NN)
should be defined in the calling environment."
  (list

    ;------------------------------------------------------------------------
    ;  1  Core elements
    ;------------------------------------------------------------------------
    `(default                          :foreground ,c-09
                                       :background ,c-00)
    `(cursor                           :background ,c-0A)
    `(region                           :background ,c-02) ; See note
    `(vertical-border                  :foreground ,c-04
                                       :background ,c-04)

    ;------------------------------------------------------------------------
    ;  2  Font types
    ;------------------------------------------------------------------------
    `(font-lock-constant-face          :foreground ,c-23)
    `(font-lock-function-name-face     :foreground ,c-28)
    `(font-lock-keyword-face           :foreground ,c-27
                                       :weight     normal)
    `(font-lock-preprocessor-face      :foreground ,c-19)
    `(font-lock-string-face            :foreground ,c-26)
    `(font-lock-type-face              :foreground ,c-1B)
    `(font-lock-variable-name-face     :foreground ,c-25
                                       :underline  nil)
    `(font-lock-warning-face           :foreground ,c-2A)

    `(font-class-highlight             :foreground ,c-15)
    `(font-class-signal                :foreground ,c-11)
    `(font-class-muted                 :foreground ,c-24)

    `(font-class-warning-bg            :foreground ,c-15
                                       :background ,c-29)
    `(font-class-error                 :foreground ,c-18)
    `(font-class-success               :foreground ,c-1A)

    `(font-class-mode-line             :foreground ,c-00
                                       :background ,c-0A
                                       :box        (:line-width 1
                                                    :color ,c-0A))
    `(font-class-mode-line-inactive    :foreground ,c-0B
                                       :background ,c-03
                                       :box        (:line-width 1
                                                    :color ,c-03))
    `(font-class-peripheral            :foreground ,c-06
                                       :inherit    default)
    `(font-class-peripheral-highlight  :foreground ,c-0A)
    `(font-class-button                :foreground ,c-11
                                       :background ,c-03
                                       :height     0.9
                                       :box        (:line-width 1
                                                    :color nil
                                                    :style released-button))

    `(font-class-heading               :foreground ,c-11)
    `(font-class-heading-segment       :foreground ,c-1A)
    `(font-class-link                  :foreground ,c-2B
                                       :underline  t)
    `(font-class-link-visited          :foreground ,c-30
                                       :underline  t)

    `(font-class-select-primary        :foreground ,c-00
                                       :background ,c-15)
    `(font-class-select-region         :background ,c-02)
    `(font-class-select-region-distinct :background ,c-03)
    `(font-class-select-secondary      :foreground ,c-00
                                       :background ,c-19)

    `(font-class-declaration           :foreground ,c-31)

    `(font-class-added                 :foreground ,c-21)
    `(font-class-added-reverse         :foreground ,c-13
                                       :background ,c-20)
    `(font-class-changed               :foreground ,c-23)
    `(font-class-changed-reverse       :foreground ,c-13
                                       :background ,c-22)
    `(font-class-removed               :foreground ,c-17)
    `(font-class-removed-reverse       :foreground ,c-13
                                       :background ,c-16)

    ;------------------------------------------------------------------------
    ;  3  Specialized domains
    ;------------------------------------------------------------------------
    ;  highlight-indent-guides
    `(highlight-indent-guides-character-face :foreground ,c-03)

    ;  Magit
    `(magit-diff-context-highlight     :background ,c-02) ; See note

    ;------------------------------------------------------------------------
    ;  4  Emacs internal
    ;------------------------------------------------------------------------
    ;  UI devices
    `(widget-field                     :background ,c-03
                                       :foreground ,c-0B)
  )
)
;____________________________________________________________________________



;============================================================================
;  Provide
;----------------------------------------------------------------------------
(provide 'theme-base)
;____________________________________________________________________________



;;|EOF
