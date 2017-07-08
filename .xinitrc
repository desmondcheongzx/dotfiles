#xrandr --dpi 96
#xrdb -merge ~/.Xresources
xset -b #disable terminal beeps
feh --bg-fill ~/wallpaper.png
setxkbmap -option "caps:ctrl_modifier"
xdotool mousemove 1440 900
#udiskie -2 &
mpd
#xbacklight -set 70%

exec i3
