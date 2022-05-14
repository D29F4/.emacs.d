; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Functions: editing
;;
;;===========================================================================



;============================================================================
;  Reverse indent
;
;  Source: https://stackoverflow.com/questions/23692879
;          https://stackoverflow.com/questions/2249955
;----------------------------------------------------------------------------
(defun reverse-indent ()
  "Reverse indent."
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      (cond
        ;((looking-at "^    ")
        ((looking-at (concat "^" (make-string tab-width ?\ )))
         (replace-match ""))
        ((looking-at "^\s+")
         (replace-match ""))
      ))))
;____________________________________________________________________________



;============================================================================
;  unfill-paragraph
;
;  Source: http://nullman.net/emacs/files/init-emacs.el.html
;----------------------------------------------------------------------------
(defun unfill-paragraph ()
  "Convert a multi-line paragraph into a single line of text."
  (interactive)
  (let
    ((fill-column (point-max)))
    (fill-paragraph nil)
  )
)
;____________________________________________________________________________



;============================================================================
;  Enhancement of standard delete-horizontal-space
;
;
;  Would prefer a simpler design; namely: abstract the forms which call
;  skip-chars-* and constrain-to-field in some fashion (macros,
;  perhaps)....
;
;    ;  Obtain preceding limit position
;    (defmacro backward-skip () (progn
;      `(skip-chars-backward ,skip-string)
;      `(constrain-to-field nil ,orig-pos)
;    ))
;     Obtain following limit position
;    (defmacro forward-skip () (progn
;      `(skip-chars-forward ,skip-string)
;      `(constrain-to-field nil ,orig-pos t)
;    ))
;
;  Then again: it may simply be more efficient for this very small function
;  to simply duplicate those forms.
;
;
;  If for safety a separate function is desired the following shall
;  suffice:
;
;    (defun delete-horizontal-space-after ()
;      "Delete all spaces and tabs after (to the right of) point.
;    To delete whitespace before point or around point use the
;    standard `delete-horizontal-space'.
;
;    We would prefer to incorporate the present functionality into
;    `delete-horizontal-space' as a third option but doing so could
;    result in incompatibility."
;      (interactive)
;      (let ((orig-pos (point)))
;        (delete-region
;          (progn
;            (skip-chars-forward " \t")
;            (constrain-to-field nil orig-pos t))
;          orig-pos)))
;
;  (Quotation marks in the above should be unescaped.)
;----------------------------------------------------------------------------
(defun delete-horizontal-space (&optional divided-scope)
  "Delete spaces and tabs before, after, or surrounding point.

The standard `delete-horizontal-space' deletes either all
surrounding whitespace (when it is called without its optional
argument, backward-only, or when said argument is nil) or the
space preceding point (when backward-only is t).

This function replaces the original in order to incorporate a
third option---the obvious deletion of whitespace following
point.  To that end the optional argument has been renamed (to
DIVIDED-SCOPE).

The functionality is as follows:

  . If the optional DIVIDED-SCOPE is absent or nil then all
    surrounding whitespace is deleted.

  . If DIVIDED-SCOPE < 0 then whitespace following point is
    deleted.

  . If DIVIDED-SCOPE is otherwise non-nil then whitespace
    preceding point is deleted.

Requiring the specification of a particular non-nil value in
order to effect the new functionality preserves the original
intent in an effort to avoid incompatibility.  As long as any
call expecting the standard Emacs definition supplies a strict t
value for the argument the behavior here will be identical."
  (interactive "*P")
  (let*
    (
      ;  The current point position
      (orig-pos (point))
      ;  Argument to skip-chars-(for|back)ward
      (skip-string " \t")
    )

    ;  Delete the region
    (apply 'delete-region
      (if divided-scope
        ;  divided-scope is non-nil
        (list
          ;  Start position
          orig-pos
          ;  End position
          (if (and (integerp divided-scope) (< divided-scope 0))
            ;  Delete space after point
            (progn
              (skip-chars-forward skip-string)
              (constrain-to-field nil orig-pos t)
            )
            ;  divided-scope can be t; delete space before point
            (progn
              (skip-chars-backward skip-string)
              (constrain-to-field nil orig-pos)
            )
          )
        )

        ;  divided-scope is nil; delete all surrounding space
        (list
          ;  Start position
          (progn
            (skip-chars-forward skip-string)
            (constrain-to-field nil orig-pos t)
          )
          ;  End position
          (progn
            (skip-chars-backward skip-string)
            (constrain-to-field nil orig-pos)
          )
        )
      )
    )
  )
)
;____________________________________________________________________________



;;|EOF