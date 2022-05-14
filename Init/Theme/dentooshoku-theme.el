; SPDX-License-Identifier: MIT
;;===========================================================================
;;
;;  dentooshoku-theme.el
;;
;;  Emacs theme: dentooshoku
;;
;;  Theme drawing from one representation of what is known as the catalog of
;;  traditional Japanese (dentoshooku) colors.
;;
;;  Colors from https://nipponcolors.com/, which cites
;;    _日本の伝統色_ (The Traditional Colors of Japan)
;;    浜田 信義 (Hamada, Nobuyoshi)
;;    パイインターナショナル (2011)
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
(deftheme dentooshoku
  "Emacs theme: dentooshoku")
;____________________________________________________________________________



;============================================================================
;  Definition
;----------------------------------------------------------------------------
;  Initialize internal associations
(custom-theme-default-init-map 'dentooshoku)

;  Define and set colors
(let
  (
    ;------------------------------------------------------------------------
    ;  Simple colors
    ;------------------------------------------------------------------------
    ;  Greys and terminals
    (c-00  "#000000") ; Black
    (c-01  "#0C0C0C") ; Grey 0        ; 呂　　　(Ryo)
    (c-02  "#1C1C1C") ; Grey 1        ; 墨　　　(Sumi)
    (c-03  "#2F2F2F") ; Grey 2
    (c-04  "#373C38") ; Grey 3        ; 藍墨茶　(Aisumicha)
    ;(c-05 "#434343") ; Grey 4        ; 消炭　　(Keshizumi)
    (c-06  "#535953") ; Grey 5        ; 青鈍　　(Aonibi)
    (c-07  "#656765") ; Grey 6        ; 鈍　　　(Nibi)
    ;(c-08 "#787878") ; Grey 7        ; 鉛　　　(Namari)
    (c-09  "#868686") ; Grey 8
    (c-0A  "#91989F") ; Grey 9        ; 銀鼠　　(Ginnezumi)
    (c-0B  "#A3A3A3") ; Grey A
    ;(c-10 "#B3B3B3") ; Grey B
    (c-11  "#BDC0BA") ; Grey C        ; 白鼠　　(Shironezumi)
    ;(c-12 "#D6D6D6") ; Grey D
    (c-13  "#E6D6D6") ; Grey E
    (c-14  "#F3F3F3") ; Grey F
    (c-15  "#FFFFFB")                 ; 胡粉　　(Gofun)

    ;  Hues
    (c-16  "#8E354A")                 ; 蘇芳　　(Suoh)
    (c-17  "#9F353A")                 ; 燕脂　　(Enji)
    (c-18  "#CB1B45")                 ; 紅　　　(Kurenai)
    (c-19  "#A28C37")                 ; 菜種油　(Nataneyu)
    (c-1A  "#EFBB24")                 ; 鬱金　　(Ukon)
    (c-1B  "#70649A")                 ; 二藍　　(Futaai)
    (c-20  "#516E41")                 ; 青丹　　(Aoni)
    (c-21  "#7BA23F")                 ; 萌黄　　(Moegi)
    (c-22  "#0B346E")                 ; 瑠璃紺　(Rurikon)
    (c-23  "#986DB2")                 ; 半　　　(Hashita)
    (c-24  "#72636E")                 ; 鳩羽鼠　(Hatobanezumi)
    (c-25  "#6699A1")                 ; 錆浅葱　(Sabiasagi)
    (c-26  "#566C73")                 ; 藍鼠　　(Ainezumi)
    (c-27  "#2EA9DF")                 ; 空　　　(Sora)
    (c-28  "#A5DEE4")                 ; 瓶覗　　(Kamenozoki)
    (c-29  "#93C3A3")                 ; 白緑　　(Byakuroku) (transformation)
    (c-2A  "#A8F8B9")                 ; 白緑　　(Byakuroku) (transformation)
    (c-2B  "#64709A")                 ; 二藍　　(Futaai) (transformation)
    (c-30  "#70649A")                 ; 二藍　　(Futaai)
    ;(c-31 "#5DAC81")                 ; 若竹　　(Wakatake)
    (c-31  "#7BA23F")                 ; 萌黄　　(Moegi)
  )

  ;--------------------------------------------------------------------------
  ;  Engage custom-theme-set-faces
  ;--------------------------------------------------------------------------
  (apply
    'custom-theme-set-faces
    'dentooshoku
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
(provide-theme 'dentooshoku)
(provide       'dentooshoku-theme)
;____________________________________________________________________________



;;|EOF
