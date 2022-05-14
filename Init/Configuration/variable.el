; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Variables
;;
;;===========================================================================



;============================================================================
;  General and Various
;----------------------------------------------------------------------------
;  tab-width: The width of an actual TAB character (and not spaces which
;             might be inserted by pressing the <TAB> key).  This does not
;             affect tab-stop-list.  TAB characters are disabled here by
;             setting indent-tabs-mode to nil.
;----------------------------------------------------------------------------
(blink-cursor-mode                -1)
(menu-bar-mode                    -1)
(save-place-mode                   1)
(scroll-bar-mode                  -1)
(show-paren-mode                   t)
(tool-bar-mode                    -1)
(tooltip-mode                     -1)
(setq-default
  confirm-kill-emacs               'y-or-n-p
  delete-trailing-lines            nil
  electric-indent-mode             nil
  fill-column                      77
  help-window-select               t
  indent-tabs-mode                 nil
  inhibit-startup-screen           t
  initial-major-mode               'text-extra-mode
  initial-scratch-message          nil
  insert-default-directory         nil
  line-number-display-limit        65535
  major-mode                       'text-extra-mode
  mode-require-final-newline       nil
  mouse-wheel-mode                 t
  ring-bell-function               'ignore
  show-trailing-whitespace         t
  tab-width                        2
  transient-mark-mode              t
  truncate-lines                   t
  vc-make-backup-files             t
  view-read-only                   t
  whitespace-line-column           fill-column
  x-select-enable-clipboard        t
)
;  tab-stop-list
;    '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80)
(defalias 'yes-or-no-p 'y-or-n-p)
;____________________________________________________________________________



;============================================================================
;  Customize
;----------------------------------------------------------------------------
(setq-default widget-image-enable nil)
;____________________________________________________________________________



;============================================================================
;  Mode Line
;----------------------------------------------------------------------------
;  Set the position to indicate columns and lines in a coordinates-style
;  format.  Note that this overrides settings which manage the presence and
;  format of the column value.
;    Setting column-number-mode to t is not necessary if this is so defined.
(setq-default mode-line-position
  (list mode-line-percent-position "  %c\u00B7%l ") ; U+00B7: MIDDLE DOT
)
;____________________________________________________________________________



;============================================================================
;  Saving
;----------------------------------------------------------------------------
;  Hooks
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'time-stamp)

;  Default file modes (in lieu of observing umask value)
(set-default-file-modes #o770)
;____________________________________________________________________________



;============================================================================
;  Scrolling
;----------------------------------------------------------------------------
(setq-default
  ;  Enable "smooth" scrolling
  ;  scroll-conservatively is the first priority amongst various scrolling options
  scroll-conservatively 10000
  ;scroll-step 1 ; An alternative to a scroll-conservatively setting

  ;  Preserve point position when returning to visited screen
  scroll-preserve-screen-position t

  ;  Establish marginal buffer when scrolling
  scroll-margin 3
)
;____________________________________________________________________________



;============================================================================
;  Time Stamps
;----------------------------------------------------------------------------
(setq-default
  time-stamp-line-limit 4
  time-stamp-start      "[ ]+\\\\?[\"<]+"
  time-stamp-end        ">"
  time-stamp-format     "%:y-%02m-%02dT%02H:%02M:%02S"
)
;____________________________________________________________________________



;============================================================================
;  Uniquify (force unique buffer names)
;----------------------------------------------------------------------------
(setq-default
  uniquify-buffer-name-style    'reverse
  uniquify-separator            "|"
  uniquify-after-kill-buffer-p  t
  uniquify-ignore-buffers-re    "^\\*"
)
;____________________________________________________________________________



;;|EOF
