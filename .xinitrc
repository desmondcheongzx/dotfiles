#xrandr --dpi 96
#xrdb -merge ~/.Xresources
xset -b #disable terminal beeps
feh --bg-fill ~/wallpaper.png
setxkbmap -option "caps:ctrl_modifier"
xdotool mousemove 1440 900
redshift -l 1.3667:103.8 &
#udiskie -2 &
#mpd
#xbacklight -set 70%

#for chinese pinyin input
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
ibus-daemon -drx

exec i3
