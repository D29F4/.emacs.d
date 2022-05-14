; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Key Bindings
;;
;;---------------------------------------------------------------------------
;;  General forms:
;;    (global-set-key (kbd "C-S-<down>") 'function-call)
;;    (global-set-key [?\C-S-down]       'function-call)
;;    (global-set-key [(ctrl down)]      'function-call)
;;    (global-set-key "\C-xq"            'function-call) ; (kbd C-x q)
;;  See https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html.
;;  We're lucky; it could be even more confusing than that.
;;
;;  Define a key-binding for a specific mode:
;;    (define-key lisp-mode-map (kbd "C-M-=") 'make-symbolic-link)
;;===========================================================================




;============================================================================
;  General
;============================================================================

;============================================================================
;  Buffers
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
;  Ignore specified buffers
;
;  Source: https://emacs.stackexchange.com/questions/17687
;----------------------------------------------------------------------------
(global-set-key [remap next-buffer]     'my-next-buffer)
(global-set-key [remap previous-buffer] 'my-previous-buffer)

;----------------------------------------------------------------------------
;  Switching
;----------------------------------------------------------------------------
;  Simple navigation
(global-set-key (kbd "M-<left>")  'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)

;  Visit scratch buffer
(global-set-key (kbd "C-x 9") (lambda () (interactive) (switch-to-buffer "*scratch*")))

;----------------------------------------------------------------------------
;  Swapping
;----------------------------------------------------------------------------
;  Swap buffers (and leave point in current window)
(global-set-key (kbd "C-S-<left>")    (lambda () (interactive) (swap-buffer-right)))
(global-set-key (kbd "C-S-<right>")   (lambda () (interactive) (swap-buffer-left)))
(global-set-key (kbd "C-S-<up>")      (lambda () (interactive) (swap-buffer-down)))
(global-set-key (kbd "C-S-<down>")    (lambda () (interactive) (swap-buffer-up)))

;  Swap buffers (and move point with starting buffer)
(global-set-key (kbd "C-S-M-<left>")  (lambda () (interactive) (swap-buffer-and-move-left)))
(global-set-key (kbd "C-S-M-<right>") (lambda () (interactive) (swap-buffer-and-move-right)))
(global-set-key (kbd "C-S-M-<up>")    (lambda () (interactive) (swap-buffer-and-move-up)))
(global-set-key (kbd "C-S-M-<down>")  (lambda () (interactive) (swap-buffer-and-move-down)))

;----------------------------------------------------------------------------
;  Destruction
;----------------------------------------------------------------------------
;  Kill the current buffer
;
;  Since the desired combination (C-x C-x) is standardly bound: first rebind
;  that functionality and then assign kill-buffer as intended.
(global-set-key (kbd "C-x x")   (lambda () (interactive) (exchange-point-and-mark)))
(global-set-key (kbd "C-x C-x") (lambda () (interactive) (kill-buffer)))

;  Kill various selections of buffers
(global-set-key (kbd "s-a") (lambda () (interactive) (kill-buffers-selective 'all)))
(global-set-key (kbd "s-o") (lambda () (interactive) (kill-buffers-selective 'other)))
(global-set-key (kbd "s-d") (lambda () (interactive) (kill-buffers-selective 'dired)))
(global-set-key (kbd "s-n") (lambda () (interactive) (kill-buffers-selective 'nonfile)))
(global-set-key (kbd "s-s") (lambda () (interactive) (kill-buffers-selective 'system)))
;____________________________________________________________________________



;============================================================================
;  Buffer modes
;----------------------------------------------------------------------------
;(global-set-key [C-left] 'backward-word-dwim) ; from motion-and-kill-dwim.el

;----------------------------------------------------------------------------
;  Toggle view-scroll-mode
;
;  (Assuming that the Caps Lock key is rendered inactive with setxkbmap's
;  caps:none option; in that condition Emacs knows <Caps_Lock> as
;  <VoidSymbol>.)
;----------------------------------------------------------------------------
(global-set-key (kbd "<Scroll_Lock>") 'toggle-view-scroll-modes)
(global-set-key (kbd "<VoidSymbol>")  'toggle-view-scroll-modes)
;____________________________________________________________________________



;============================================================================
;  Lines
;
;  [M-;]: call comment-dwim with `1` as argument for only one comment
;         character.  (Overriding existing key-binding.)
;----------------------------------------------------------------------------
(global-set-key (kbd "<backtab>") 'reverse-indent)
(global-set-key (kbd "C-<tab>")   'indent-rigidly)
(global-set-key (kbd "C-S-k")     'kill-whole-line)
(global-set-key (kbd "C-x t")     'toggle-truncate-lines)
(global-set-key (kbd "M-;")       (lambda () (interactive) (comment-dwim 1)))
(global-set-key (kbd "M-S-<backspace>")
  (lambda () (interactive) (delete-horizontal-space -1)))

;  Package move-text: set bindings for shifting text vertically
(move-text-default-bindings)
;____________________________________________________________________________



;============================================================================
;  Keyboard macros
;----------------------------------------------------------------------------
(global-set-key (kbd "C-x C-k 1") 'insert-text-format-section)
(global-set-key (kbd "C-x C-k 2") 'insert-date-current)
;____________________________________________________________________________



;============================================================================
;  Windows
;----------------------------------------------------------------------------
;  Creation
;----------------------------------------------------------------------------
(global-set-key (kbd "s-\\")        'cast-unbalanced-windows)

;----------------------------------------------------------------------------
;  Traversal
;
;  (Earlier had considered keypad keys: <kp-x>.)
;----------------------------------------------------------------------------
(global-set-key (kbd "s-<left>")    'windmove-left)
(global-set-key (kbd "s-<right>")   'windmove-right)
(global-set-key (kbd "s-<up>")      'windmove-up)
(global-set-key (kbd "s-<down>")    'windmove-down)
;____________________________________________________________________________




;============================================================================
;  Mode-specific
;============================================================================

;============================================================================
;  Ibuffer
;----------------------------------------------------------------------------
;  Make ibuffer the default buffer menu
(global-set-key (kbd "C-x C-b") 'ibuffer)
;____________________________________________________________________________



;============================================================================
;  Dired
;----------------------------------------------------------------------------
(defun add-to-dired-mode-map ()
  "Contribute definitions to dired-mode-map."
  ;  Keybinding to visit parent directory after killing current dired buffer.
  ;  (Adapting body of dired-find-alternate-file (which see).  (Can simply
  ;  assign to dired-up-directory if killing current buffer is not desired.))
  (define-key dired-mode-map (kbd "\\")
    (lambda ()
      (interactive)
      (set-buffer-modified-p nil)
      (find-alternate-file "..")))
)

;  Prepare dired-mode-map
(if (boundp 'dired-mode-map)
  ;  If dired is already loaded then add the keymap bindings
  (add-to-dired-mode-map)
  ;  Dired is not loaded; add bindings to the load hook
  (add-hook 'dired-load-hook 'add-to-dired-mode-map)
)
;____________________________________________________________________________



;============================================================================
;  highlight-indent-guides
;----------------------------------------------------------------------------
;  Toggle highlight-indent-guides-mode
(global-set-key (kbd "C-c g") 'highlight-indent-guides-mode)
;____________________________________________________________________________



;============================================================================
;  Magit
;----------------------------------------------------------------------------
(global-set-key (kbd "C-x g") 'magit-status)
;____________________________________________________________________________



;;|EOF
