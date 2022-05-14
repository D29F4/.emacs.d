<!-- SPDX-License-Identifier: MIT -->

#  Themes

(Some revision may still be warranted here.)


##  Usage

###  Overview

This theme is intended to act only as a foundation for actual theme
instantiations.  It provides core face/attribute relationships which shall be
applied to all instantiations.  Faces are specified in two ways:

  1. Known attributes and values are assigned to some faces (in the Face
     Inheritance section).  These assignments primarily prescribe inheritance
     relationships.

  2. Placeholder color variables representing particular color values are
     assigned to faces (in the Default Color Map section).  The color variables
     have names in the form `c-XX`, where "XX" is a two-digit duodecimal number.

     It is expected that the instantiating themes shall provide the placeholder
     variables with appropriate color values.  Examine the foundational file to
     determine which variables are in use and their application.


###  Theme Instantiations

An instantiating file simply needs to load the fundamental theme, define itself,
assign the foundational default map via custom-theme-set-faces, and (finally)
officially provide itself.  Essentially:

```lisp
(eval-and-compile
  (require 'theme-base))

(deftheme name-of-instantiation
  "Emacs theme: name-of-instantiation")

(custom-theme-default-init-map 'name-of-instantiation)

(let
  (
    (c-00 "#123123") (c-01 "colorname") ; Etc.
  )
  (apply
    'custom-theme-set-faces
    'name-of-instantiation
    ;  See custom-theme-construct-faces in the foundation file
    (custom-theme-construct-faces
      '((class color) (min-colors 89))

      ;  This returns the face assignments in (2) above as a list
      (custom-theme-default-color-map)
    )
  )
)
(provide-theme 'name-of-instantiation)
(provide 'name-of-instantiation-theme)
```


###  Instantiation-specific Variations

To introduce new faces or override previously-defined ones simply append a new
face list to the output of custom-theme-default-color-map.

Essentially: replace in the `apply` form above

```lisp
(custom-theme-default-color-map)
```

with, for example:

```lisp
(append
  (custom-theme-default-color-map)
  (list
    `(face1 :foreground "#000000")
    `(face2 :background ,color-05)
    `(face3 :inherit    font-lock-keyword-face)
  )
)
```
