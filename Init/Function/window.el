; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Functions: window management
;;
;;===========================================================================



;============================================================================
;  quit-window
;
;  Copied with minimal revision from window.el (q.v.).  Modified solely in
;  order to reverse the default and optional functions: that is: to make
;  killing the default and burying the alternative.  The standard behavior is
;  utterly stupid.
;----------------------------------------------------------------------------
(defun quit-window (&optional bury window)
  "Kill WINDOW and kill its buffer.
WINDOW must be a live window and defaults to the selected one.
With prefix argument BURY non-nil, bury the buffer instead of
killing it.

According to information stored in WINDOW's `quit-restore' window
parameter either (1) delete WINDOW and its frame, (2) delete
WINDOW, (3) restore the buffer previously displayed in WINDOW,
or (4) make WINDOW display some other buffer than the present
one.  If non-nil, reset `quit-restore' parameter to nil."
  (interactive "P")
  (quit-restore-window window (if bury 'bury 'kill)))
;____________________________________________________________________________



;============================================================================
;  cast-unbalanced-windows
;----------------------------------------------------------------------------
(defun cast-unbalanced-windows ()
  "Create an unbalanced window arrangement.
Deletes other windows and then splits with a narrow left-side pane."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (shrink-window-horizontally 48)
  (switch-to-next-buffer)
  (windmove-right)
)
;____________________________________________________________________________



;;|EOF
