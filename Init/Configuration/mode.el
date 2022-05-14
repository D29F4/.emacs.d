; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Loading and Configuring Modes
;;
;;===========================================================================



;============================================================================
;  General
;============================================================================

;============================================================================
;  Keyword Formatting
;----------------------------------------------------------------------------
;  Using lambda function in order to treat derived modes (per documentation).
(dolist (hook (list 'text-mode-hook 'prog-mode-hook))
  (add-hook hook
    (lambda ()
      ;  Additional keyword highlighting (comment keys)
      (font-lock-add-keywords nil
        (quote (
          ("|FIX" 0 'font-lock-warning-face t)
          ;("\\(|\\(FIX\\|TEST\\)\\)" 1 font-class-warning t)
        ))
        t
      )
    )
  )
)
;____________________________________________________________________________



;============================================================================
;  Line Numbers
;----------------------------------------------------------------------------
;  Display line numbers for modes derived from prog-mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;____________________________________________________________________________



;============================================================================
;  Whitespace
;----------------------------------------------------------------------------
;  Disable trailing whitespace in selected modes.
;  Based on https://emacs.stackexchange.com/a/40649.
(add-hook 'after-change-major-mode-hook
  (lambda ()
    (when
      (derived-mode-p
        'eww-mode
        'magit-popup-mode
        'shell-mode
      )
      (setq show-trailing-whitespace nil))))
;____________________________________________________________________________




;============================================================================
;  Mode-specific
;============================================================================

;============================================================================
;  css-mode
;----------------------------------------------------------------------------
(add-hook 'css-mode-hook
  (lambda ()
    (setq css-indent-offset 2)))
;____________________________________________________________________________



;============================================================================
;  Dired
;----------------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)

(setq-default dired-listing-switches
  "-al --time-style=long-iso --group-directories-first")
  ;"-algG --time-style=+ --group-directories-first")  ; limited dired
;____________________________________________________________________________



;============================================================================
;  git-gutter
;----------------------------------------------------------------------------
;  Establish presence
;(global-git-gutter-mode t)
(add-hook 'prog-mode-hook 'git-gutter-mode)

(setq-default
  ;  Text for minor-mode indicator in mode line (include an initial space)
  git-gutter:lighter ""

  ;  Delay for status updates
  git-gutter:update-interval 2

  ;  Status indicators
  git-gutter:added-sign    " "
  git-gutter:modified-sign " "
  git-gutter:deleted-sign  " "
)
;____________________________________________________________________________



;============================================================================
;  highlight-indent-guides
;----------------------------------------------------------------------------
(setq-default
  ;  Format guides as a character (the default long vertical stroke)
  highlight-indent-guides-method                   'character

  ;  Confirm the expected default (no responsiveness)
  highlight-indent-guides-responsive               nil

  ;  Expect a theme to prescribe a face value
  highlight-indent-guides-auto-enabled             nil

  ;  Supply a reasonable value for the automatic face anyway
  highlight-indent-guides-auto-character-face-perc 30
)
;____________________________________________________________________________



;============================================================================
;  html-mode
;----------------------------------------------------------------------------
(add-hook 'html-mode-hook
  (lambda ()
    (auto-fill-mode -1)))
;____________________________________________________________________________



;============================================================================
;  Ibuffer
;----------------------------------------------------------------------------
(setq-default

  ;  Redefine (in order to reorder) available formats
  ibuffer-formats
  '(
    (
      mark
      modified
      " "
      ;(name 16 -1)
      (name 16 16 :left :elide)
      " "
      filename
    )
    (
      mark
      modified
      " "
      name
    )
    (
      mark
      modified
      read-only
      locked
      " "
      (name 18 18 :left :elide)
      " "
      (size  9 -1 :right)
      " "
      (mode 16 16 :left :elide)
      " "
      filename-and-process
    )
  )

  ;  Establish Ibuffer filter groups
  ibuffer-saved-filter-groups
  '(
    ("Default"
      ; ("//Current"
      ;   (filename . "0/Current"))
      ("Emacs"
        ;(name    . "^\\*(scratch|Messages|Help|Apropos|info)\\*$")
        (name . "^\\*")
      )
      ("Magit"
        ;(or (mode     . magit-status-mode)
        ;    (mode     . magit-diff-mode)
        ;    (mode     . magit-log-mode)))
        (name . "^magit") ; A relatively poor solution
      )
      ("Dired"
        (mode . dired-mode)
      )
    )
  )

  ;  Hide empty buffer groups
  ibuffer-show-empty-filter-groups nil

  ;  Hide concluding summary line
  ibuffer-display-summary nil

  ;  Act without prompts for some actions
  ibuffer-expert t
)

;  Ibuffer mode hook
(add-hook 'ibuffer-mode-hook (lambda ()

  ;  Automatic updates of buffer list
  (ibuffer-auto-mode t)

  ;  Use defined default buffer group
  (ibuffer-switch-to-saved-filter-groups "Default")
))
;____________________________________________________________________________



;============================================================================
;  Magit
;----------------------------------------------------------------------------
(setq-default
  magit-section-highlight-hook nil
  magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n16"))
  magit-log-margin    (quote (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
)
;____________________________________________________________________________



; ;============================================================================
; ;  Markdown
; ;----------------------------------------------------------------------------
; (add-to-list 'markdown-code-lang-modes `("latex" . tex-mode))
; (add-to-list 'markdown-code-lang-modes `("raku" . raku-mode))
; (add-to-list 'markdown-code-lang-modes `("tex" . tex-mode))
; ;____________________________________________________________________________



;============================================================================
;  Minibuffer
;----------------------------------------------------------------------------
;  Removes *Completions* from buffer after opening a file.
;  Source: https://unix.stackexchange.com/a/152151
(add-hook 'minibuffer-exit-hook
  (lambda ()
    (let
      (
        (buffer "*Completions*")
      )
      (and
        (get-buffer buffer)
        (kill-buffer buffer)
      )
    )
  )
)
;____________________________________________________________________________



;============================================================================
;  perl-mode
;----------------------------------------------------------------------------
;  perl-brace-offset
(setq-default perl-brace-offset 0)

; ;  Remove the comma's association with electric-perl-terminator.
; (define-key perl-mode-map ";" nil)

(add-hook 'perl-mode-hook
  (lambda ()
    ;  Use "#" instead of perl-mode's default "# "
    (setq comment-start "#")

    ;  Set indent level
    (setq perl-indent-level 2)
  )
)

;  Additional Keywords
(font-lock-add-keywords
  'perl-mode
  '(("\\<given\\|when\\>" . font-lock-keyword-face))
  t
)
;____________________________________________________________________________



;============================================================================
;  python-mode
;----------------------------------------------------------------------------
(setq-default python-indent-offset 2)
;____________________________________________________________________________



;============================================================================
;  raku-mode
;----------------------------------------------------------------------------
(setq-default raku-indent-offset 2)
;____________________________________________________________________________



;============================================================================
;  tex-mode
;----------------------------------------------------------------------------
;  Remap faces for TeX mode.  (See documentation for face-remapping-alist.)
(add-hook 'tex-mode-hook
  (lambda ()
    (set
      (make-local-variable 'face-remapping-alist)
      '((font-lock-keyword-face font-lock-variable-name-face)
        (font-lock-constant-face font-lock-function-name-face))
    )
    (redraw-display)
  )
)
;____________________________________________________________________________



;============================================================================
;  text-mode
;----------------------------------------------------------------------------
(define-key text-mode-map (kbd "TAB") 'tab-to-tab-stop)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
;____________________________________________________________________________



;============================================================================
;  typescript-mode
;----------------------------------------------------------------------------
(add-hook 'typescript-mode-hook
  (lambda ()
    (setq typescript-indent-level 2)))
;____________________________________________________________________________



;============================================================================
;  view-mode
;----------------------------------------------------------------------------
;  Activate and deactivate scroll-lock-mode in conjunction with view-mode
(add-hook 'view-mode-hook
  (lambda ()
    (scroll-lock-mode (if (eval view-mode) 1 0))))
;____________________________________________________________________________



;;|EOF
