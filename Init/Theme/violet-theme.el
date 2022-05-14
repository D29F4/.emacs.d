; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  violet-theme.el
;;
;;  Emacs theme: violet
;;
;;===========================================================================



;============================================================================
;  Inheritance
;----------------------------------------------------------------------------
(eval-and-compile
  (require 'theme-base))
;____________________________________________________________________________



;============================================================================
;  Initialization
;----------------------------------------------------------------------------
(deftheme violet
  "Emacs theme: violet")
;____________________________________________________________________________



;============================================================================
;  Definition
;----------------------------------------------------------------------------
;  Initialize internal associations
(custom-theme-default-init-map 'violet)

;  Define and set colors
(let
  ;--------------------------------------------------------------------------
  ;  Variables
  ;--------------------------------------------------------------------------
  (
    ;  Greys and terminals
    (c-00  "#000000") ; Black
    (c-01  "#0C0C0C") ; Grey 0
    (c-02  "#1F1F1F") ; Grey 1
    (c-03  "#2F2F2F") ; Grey 2
    (c-04  "#3C3C3C") ; Grey 3
    ;(c-05 "#494949") ; Grey 4
    (c-06  "#535353") ; Grey 5
    (c-07  "#636363") ; Grey 6
    ;(c-08 "#767676") ; Grey 7
    (c-09  "#868686") ; Grey 8
    (c-0A  "#939393") ; Grey 9
    (c-0B  "#A3A3A3") ; Grey A
    ;(c-10 "#B3B3B3") ; Grey B
    (c-11  "#C6C6C6") ; Grey C
    ;(c-12 "#D6D6D6") ; Grey D
    (c-13  "#E6D6D6") ; Grey E
    ;(c-14 "#F3F3F3") ; Grey F
    (c-15  "#FFFFFF") ; White

    ;  Hues
    (c-16  "#630C3C") ; Red (dark)
    (c-17  "#933C6C") ; Red (moderate)     ; #93466F; #963656; #A36; #C66
    (c-18  "#D00030") ; Red (bright)
    (c-19  "#A08000") ; Gold (pale)
    (c-1A  "#C0A020") ; Gold               ; Consider addition of #EEDC82 (LightGoldenrod2) or similar?
    (c-1B  "#A676E6") ; Purple             ; #86B6A6 (Green (sea))
    (c-20  "#304010") ; Green (dark)       ; #451; #242
    (c-21  "#5C8333") ; Green              ; #697F33; #468646; #3F7F3F; #693; #6C6
    (c-22  "#304060") ; Blue (dark)        ; #856090
    (c-23  "#7080A0") ; Blue (light)       ; #679; #89A
    (c-24  "#696669") ; Violet (dim)       ; #6F6973; #6C6679; #6F6F73; #6F636F; #696F69
    (c-25  "#866686") ; Violet (dark)
    (c-26  "#8F798A") ; Violet (medium)
    (c-27  "#904090") ; Violet (saturated)
    (c-28  "#E090E0") ; Violet (bright)
    (c-29  "#800070") ; Magenta (mild)
    (c-2A  "#D020A0") ; Magenta (bright)   ; #BC4CBC; #BB5DC4; #782599
    (c-2B  "#6090C0") ; Indigo
    (c-30  "#9060C0") ; Magenta
    (c-31  "#80A0B0") ; Cyan               ; #B9B (Violet (soft bright))
  )

  ;--------------------------------------------------------------------------
  ;  Engage custom-theme-set-faces
  ;--------------------------------------------------------------------------
  (apply
    'custom-theme-set-faces
    'violet
    (custom-theme-construct-faces
      '((class color) (min-colors 89))
      (append
        (custom-theme-default-color-map)
        (list
          ;  Diff
          `(diff-file-header                 :inherit    font-class-highlight)
        )
      )
    )
  )
)
;____________________________________________________________________________



;============================================================================
;  Provide theme
;----------------------------------------------------------------------------
(provide-theme 'violet)
(provide       'violet-theme)
;____________________________________________________________________________



;;|EOF
