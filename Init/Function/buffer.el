; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Functions: buffer management
;;
;;===========================================================================



;============================================================================
;  Combined view and scroll modes
;----------------------------------------------------------------------------
(defun toggle-view-scroll-modes ()
  "Toggle view-mode and scroll-mode (as appropriate per context).

Considerations:

  . Preserve view-mode for (or newly apply it to) buffers
    instantiating read-only files.
  . view-mode should not be enabled under selected conditions;
    namely (at least for now): when dired-mode is the buffer's
    major mode.
  . The current configuration dictates that scroll-lock-mode is
    activated and deactivated in sympathy with view-mode though
    the present implementation does not need to account for that.

The intended state map for view-mode (v) and scroll-lock-mode (s):

  Initial  >   Default  Read-only  Dired
   v  s         v  s      v  s      v  s
  --------------------------------------
   0  0    >    1  1      1  1      0  1
   0  1    >    1  1      1  0      0  0
   1  0    >    1  1      1  1      0  1
   1  1    >    0  0      1  0      0  0
"
  (interactive)
  (let
    (
      ;  Note the current states as useful values.
      ;  In fact the *-mode functions here accept integer values and not
      ;  strictly t and nil; so ensure integers as return values throughout.
      (view-initial   (if (bound-and-true-p view-mode) 1 0))
      (scroll-initial (if (bound-and-true-p scroll-lock-mode) 1 0))

      ;  Whether or not to prevent activation of view-mode
      (prevent-view-mode (member major-mode (list 'dired-mode)))
    )

    ;  Set view-mode
    ;
    (view-mode
      (if prevent-view-mode
        ;  Do not enable view-mode in selected circumstances
        0
        ;  Preserve or introduce view-mode for read-only files and when
        ;  both modes are not currently active.  Otherwise: deactivate.
        (if
          (or
            (and
              (bound-and-true-p buffer-file-truename)
              (not (file-writable-p buffer-file-truename))
            )
            (not (and (= view-initial 1) (= scroll-initial 1)))
          )
          1 0
        )
      )
    )

    ;  Set scroll-lock-mode
    ;
    (scroll-lock-mode
      (if
        (or
          (= view-initial 1)
          prevent-view-mode
          (and
            (bound-and-true-p buffer-file-truename)
            (not (file-writable-p buffer-file-truename))
          )
        )
        ;  Toggle under selected conditions
        (if (= scroll-initial 0) 1 0)
        ;  Activate (or preserve an active state)
        1
      )
    )
  )
)
;____________________________________________________________________________



;============================================================================
;  swap-buffer-<direction>
;
;  Swap the current buffer with another.  Point to remain in the starting
;  buffer.
;
;  (See also native Emacs function window-swap-states.)
;----------------------------------------------------------------------------
(defun swap-buffer-left ()
  "Swap the current buffer with the one to the left.
Point remains in the starting buffer."
  (interactive)
  (buf-move-left)
  (windmove-right))

(defun swap-buffer-right ()
  "Swap the current buffer with the one to the right.
Point remains in the starting buffer."
  (interactive)
  (buf-move-right)
  (windmove-left))

(defun swap-buffer-up ()
  "Swap the current buffer with the one above.
Point remains in the starting buffer."
  (interactive)
  (buf-move-up)
  (windmove-down))

(defun swap-buffer-down ()
  "Swap the current buffer with the one below.
Point remains in the starting buffer."
  (interactive)
  (buf-move-down)
  (windmove-up))
;____________________________________________________________________________



;============================================================================
;  swap-buffer-and-move-<direction>
;
;  Swap the current buffer with another.  Point to move with the starting
;  buffer.
;
;  (See also native Emacs function window-swap-states.)
;----------------------------------------------------------------------------
(defun swap-buffer-and-move-left ()
  "Swap the current buffer with the one to the left.
Point moves with the starting buffer."
  (interactive)
  (buf-move-left))

(defun swap-buffer-and-move-right ()
  "Swap the current buffer with the one to the right.
Point moves with the starting buffer."
  (interactive)
  (buf-move-right))

(defun swap-buffer-and-move-up ()
  "Swap the current buffer with the one above.
Point moves with the starting buffer."
  (interactive)
  (buf-move-up))

(defun swap-buffer-and-move-down ()
  "Swap the current buffer with the one below.
Point moves with the starting buffer."
  (interactive)
  (buf-move-down))
;____________________________________________________________________________



;============================================================================
;  kill-buffers-selective
;
;  See https://www.emacswiki.org/emacs/KillingBuffers.
;----------------------------------------------------------------------------
(defun kill-buffers-selective (type)
  "Kills buffers according to argument TYPE.
TYPE:
  'all     : Kills all buffers.
  'other   : Kills all buffers other than the current one.
  'dired   : Kills all Dired buffers.
  'nonfile : Kills all buffers which are not a filesystem file
             or Ibuffer buffer.
  'system  : Kills all buffers which are not a filesystem file,
             Dired buffer, or Ibuffer buffer
Some options will kill Emacs's space-prefixed internal buffers."
  (interactive)
  (let ((starting-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (switch-to-buffer buffer)
      (when
        (and
          (cond
            ;  If 'other: kill other buffers
            ((eq type 'other)
             (not (eq (current-buffer) starting-buffer)))

            ;  If 'dired: kill if buffer is a Dired buffer
            ((eq type 'dired)
             (eq (buffer-local-value 'major-mode buffer) 'dired-mode))

            ;  If 'nonfile: kill if buffer is not a file or Ibuffer buffer
            ((eq type 'nonfile)
             (and (eq buffer-file-name nil)
                  (not (eq (buffer-local-value 'major-mode buffer) 'ibuffer-mode))
             ))

            ;  If 'system: kill if buffer is not a file, Dired buffer, or Ibuffer buffer
            ((eq type 'system)
             (and (eq buffer-file-name nil)
                  (not (eq (buffer-local-value 'major-mode buffer) 'dired-mode))
                  (not (eq (buffer-local-value 'major-mode buffer) 'ibuffer-mode))
             ))

            ;  Otherwise: permit
            (t t)
          )

          ;  And if buffer is still active
          (buffer-live-p buffer)
        )

        ;  If conditions met then kill this buffer
        (kill-buffer (buffer-name))
      )
    )

    ;  If starting buffer is still active then switch back to it
    (if (buffer-live-p starting-buffer) (switch-to-buffer starting-buffer))
  )
)
;____________________________________________________________________________



;============================================================================
;  Buffers: ignore specified buffers
;
;  Reference: https://emacs.stackexchange.com/questions/17687
;----------------------------------------------------------------------------
(defvar my-skippable-buffers
  '(
    "*Messages*"
    ;"*scratch*"
    ;"*Help*"
  )
  "Buffer names ignored by `my-next-buffer' and `my-previous-buffer'.")

(defun my-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) my-skippable-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun my-next-buffer ()
  "Variant of `next-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'next-buffer))

(defun my-previous-buffer ()
  "Variant of `previous-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'previous-buffer))
;____________________________________________________________________________



;;|EOF
