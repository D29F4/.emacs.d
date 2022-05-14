#  Emacs Configuration


##  Introduction

An Emacs configuration.  As is always the case with such things, it may
reasonably be expected to remain forever a work in progress.


##  Outline of Structure

- init.el<br />
  (Initialization file; imports all internal files and external packages)

- Init/Configuration
  - Keybindings
  - Mode-related settings
  - Overrides for various syntax tables
  - Identification of safe themes and invocation of active theme
  - General and sundry assignments and settings

- Init/Function
  - Buffer management
  - General editing
  - Convenient macros
  - Character search
  - Window management

- Init/Mode
  - Definition of text-extra-mode<br />
    (Currently merely text-mode with comments)

- Init/Theme
  - Theme template
  - Several instantiations using base template
