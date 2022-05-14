; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  Syntax Tables
;;
;;  Modifications to existing syntax tables.
;;
;;---------------------------------------------------------------------------
;;  References:
;;    https://www.emacswiki.org/emacs/EmacsSyntaxTable
;;    https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;;===========================================================================



;============================================================================
;  Perl mode
;----------------------------------------------------------------------------
(add-hook 'perl-mode-hook
  (lambda ()
    (modify-syntax-entry ?$ "w")
    (modify-syntax-entry ?@ "w")
    (modify-syntax-entry ?& "w")
    (modify-syntax-entry ?* "w")
    (modify-syntax-entry ?+ "w")
    (modify-syntax-entry ?- "w")
    (modify-syntax-entry ?\\ ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?< "w")
    (modify-syntax-entry ?= "w")
    (modify-syntax-entry ?> "w")
    (modify-syntax-entry ?? "w")
    (modify-syntax-entry ?| "w")
  )
)
;____________________________________________________________________________



;============================================================================
;  Raku mode
;----------------------------------------------------------------------------
(add-hook 'raku-mode-hook
  (lambda ()
    (modify-syntax-entry ?$ "w")
    (modify-syntax-entry ?@ "w")
    (modify-syntax-entry ?& "w")
    (modify-syntax-entry ?* "w")
    (modify-syntax-entry ?+ "w")
    (modify-syntax-entry ?- "w")
    (modify-syntax-entry ?\\ ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?< "w")
    (modify-syntax-entry ?= "w")
    (modify-syntax-entry ?> "w")
    (modify-syntax-entry ?? "w")
    (modify-syntax-entry ?| "w")
  )
)
;____________________________________________________________________________



;============================================================================
;  Text mode
;----------------------------------------------------------------------------
(add-hook 'text-mode-hook
  (lambda ()
    (modify-syntax-entry ?+  "w")
    (modify-syntax-entry ?*  "w")
    (modify-syntax-entry ?&  "w")
    (modify-syntax-entry ?\" ".") ; No special styling for quotations
  )
)
;____________________________________________________________________________



;;|EOF