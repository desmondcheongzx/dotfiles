#!/bin/sh

# /etc/X11/xinit/xinitrc
#
# global xinitrc file, used by all X sessions started by xinit (startx)

# invoke global X session script

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
        for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
            [ -x "$f" ] && . "$f"
        done
        unset f
fi

xrandr --dpi 96
xrdb -merge ~/.Xresources
feh --bg-fill ~/Pictures/Wallpapers/kimi-no-na-wa.png
setxkbmap -option "caps:ctrl_modifier"
xbacklight -set 70%

exec i3
