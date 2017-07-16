# $OpenBSD: dot.profile,v 1.4 2005/02/16 06:56:57 matthieu Exp $
#
# sh/ksh initialization

PATH=.:$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games

EDITOR='emacs -nw'
PAGER=less

XDG_CONFIG_HOME="$HOME/.config"

export PATH HOME TERM EDITOR PAGER XDG_CONFIG_HOME

eval $(keychain --eval --quiet id_rsa)
udiskie -2 &
mpd
