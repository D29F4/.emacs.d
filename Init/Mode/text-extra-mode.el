; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Major mode definition: text-extra-mode
;;
;;  A variant of text-mode.
;;    . Recognizes to-end-of-line comments (signalled with "#").
;;
;;  Guidance on comments from https://www.ergoemacs.org/emacs/elisp_comment_handling.html.
;;  https://www.emacswiki.org/emacs/DerivedMode provides some notes.
;;
;;===========================================================================



;============================================================================
;  Syntax Table
;----------------------------------------------------------------------------
(defvar text-extra-mode-syntax-table nil
  "Syntax table for text-extra-mode.")

(setq text-extra-mode-syntax-table
  (let
    ((synTable (make-syntax-table)))

    ; Comment: "#"
    (modify-syntax-entry ?#  "< b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)

    synTable
  )
)
;____________________________________________________________________________



;============================================================================
;  Mode Definition
;----------------------------------------------------------------------------
(define-derived-mode text-extra-mode text-mode
  "A variant of text-mode.
     . Recognizes to-end-of-line comments (signalled with '#')."

  ;  Syntax table
  :syntax-table text-extra-mode-syntax-table

  ;  Mode name
  (setq mode-name "text-x")

  ;  Comments
  (setq comment-start "#")
  (setq comment-end    "")

  ;  Mode hook
  ;(defvar text-extra-mode-hook nil)

  ; Run hooks
  (run-hooks 'text-extra-mode-hook)
)
;____________________________________________________________________________



;============================================================================
;  Provide mode
;----------------------------------------------------------------------------
(provide 'text-extra-mode)
;____________________________________________________________________________



;;|EOF
