; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Functions: keyboard macros
;;
;;===========================================================================



;============================================================================
;  Headers and Separator
;
;  : Optional text within headers.
;  : Some direction from https://github.com/defunkt/emacs/blob/master/defunkt/css.el
;----------------------------------------------------------------------------
;;;###autoload
(defun insert-text-format-section (type text)
    "Section and rule insert with optional text between header rules.
Assumes an end-of-line-style comment."
  (interactive "nType: \nsText: ")
  (let
    (
      (lf-char     "\n") ; Linefeed character
      (indentation "  ") ; Indentation for text between rules
      (border_1    "")   ; Opening rule
      (border_0    "")   ; Closing or solitary rule
      (rule1 (concat comment-start (make-string (1- fill-column) ?=)))
      (rule2 (concat comment-start (make-string (1- fill-column) ?-)))
      (rule0 (concat comment-start (make-string (1- fill-column) ?_)))
    )

    (cond
      ;  Three-line headers (1)
      (
        (= type 1)
        (setq border_1 rule1
              border_0 rule1)
      )

      ;  Three-line headers (2)
      (
        (= type 2)
        (setq border_1 rule1
              border_0 rule2)
      )

      ;  Three-line headers (3)
      (
        (= type 3)
        (setq border_1 rule2
              border_0 rule2)
      )

      ;  One-line separator
      (
        (= type 4)
        (setq border_0 (concat rule2 lf-char))
      )

      ;  Closing edge
      (
        (= type 5)
        (setq border_0 rule0)
      )
    )

    ;  Print rule(s) and text as appropriate
    (if (<= type 3)
      (progn
        (insert border_1 lf-char)
        (insert comment-start)
        (when (not (equal text "")) (insert indentation text))
        (insert lf-char)
      )
    )

    (insert border_0 lf-char)
  )
)
;____________________________________________________________________________



;============================================================================
;  Date Insertion
;----------------------------------------------------------------------------
;;;###autoload
(defun insert-date-current ()
  "Insert: current date (ISO) in the form YYYY-MM-DD."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))
)
;____________________________________________________________________________



;;|EOF