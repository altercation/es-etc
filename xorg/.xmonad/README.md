# Ethan Schoonover - xmonad.hs config

This is my live xmonad config file. It should be refactored and broken up into modules. That's for after I'm more Haskell proficient.

## Unique elements of this configuration:

* Custom tabs/tiled layouts that provide for dragging of tabs/tiled windows

* Tabs layout uses more efficient TabBarDecoration code (not Tabbed) but has been customized to hide tabs when only one window is present

* A customized NamedActions module shows all keybindings when Mod-F1 (Alt-F1 in my config) is pressed.

* Solarized throughout

## Helper scripts:

I'm using xmonad+xmobar with no other "desktop environment" so use 3 or 4 helper scripts to manage things like power, audio, wireless, and my display. These are all part of my es-bin repository. If you use this config, you'll either want those or want to change their use in this script.

## xmobar

I'm currently using XMobar decoupled from the traditional pipe setup with xmond.

