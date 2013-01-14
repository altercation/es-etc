#!/bin/sh

# xrdb and dbus need to be handled here since we're not using a
# desktop environment

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
sysmodmap=/etc/X11/xinit/Xmodmap
if [ -f $sysmodmap ]; then xmodmap $sysmodmap; fi
xset +fp /usr/share/fonts/local && xset fp rehash
xrdb -merge $HOME/.Xresources
xsetroot -cursor_name left_ptr
xsetroot -solid "#002b36"
hsetroot -solid "#002b36"
unclutter -root -idle 1 &
xset -b # turn off pc-speaker
xset -dpms # turn off dpms for now... not sure what xscreensaver will do with this

# ---------------------------------------------------------------------
# keyboard customization
# ---------------------------------------------------------------------
~/bin/keyboard-reset


# automatically turn off trackpad while typing
# cf https://wiki.archlinux.org/index.php/Touchpad_Synaptics
# TODO: not working??
# syndaemon -t -k -i 2 -d &

# D-Bus
#dbuslaunch="`which dbus-launch 2>/dev/null`"
#if [ -n "$dbuslaunch" ] && [ -x "$dbuslaunch" ] && [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
#  eval `$dbuslaunch --sh-syntax --exit-with-session`
#  eval `$dbuslaunch --auto-syntax`
#fi

# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# xsetwacom set "Wacom ISDv4 E6 Finger touch" Mode "Absolute"
# Following are experimental values to improve two finger scroll response
# taptime needs to be higher than 25, 250 worked to detect two finger press/tap 
# for doubleclick
#xsetwacom set "Wacom ISDv4 E6 Finger touch" TapTime 25
_eraser_id="$(xsetwacom --list devices | grep ERASER | sed 's/^.*id:\s*\(\w*\)\W.*$/\1/')"
_stylus_id="$(xsetwacom --list devices | grep STYLUS | sed 's/^.*id:\s*\(\w*\)\W.*$/\1/')"
_touch_id="$(xsetwacom --list devices | grep TOUCH | sed 's/^.*id:\s*\(\w*\)\W.*$/\1/')"
#xsetwacom set "Wacom ISDv4 E6 Finger touch" Suppress 1
#xsetwacom set "Wacom ISDv4 E6 Finger touch" RawSample 20
#xsetwacom set "Wacom ISDv4 E6 Finger touch" ScrollDistance 40

# eraser needs a unique button for gromit-mpx until I sort it out in a better 
# way (nuked this since it really does screw up mypaint)
#xinput --set-button-map $_eraser_id "2 1 3 4 5 6 7"

# see https://wiki.archlinux.org/index.php/ConsoleKit
# and https://bbs.archlinux.org/viewtopic.php?pid=905725#p905725
#/home/es/bin/xbindkeys-slowstart &

# must source environment for xbindkeys mouse bindings to function
# TODO: figure out why
. $HOME/.profile
#xbindkeys
#xbindkeys -f /etc/X11/xinit/xbindkeysrc
#xbindkeys -f $HOME/.xbindkeysrc -X :0

#compton &
/usr/bin/system boot
/usr/bin/xscreensaver -no-splash -no-capture-stderr &
dropboxd &
xmonad
