#!/usr/bin/env zsh
maybe-disable-lcd.sh
source ~/.zshrc
xset +fp ~/.fonts/ &
xset fp rehash &
# xcompmgr -fF -I-.002 -O-.003 -D1 &
# xcompmgr -c -t-5 -l-5 -r4.2 -o.75 &
# xcompmgr -cCfF -r7 -o.65 -l-10 -t-8 -D7 &
xrdb -merge .Xresources &
# gnome-settings-daemon &
gnome-volume-manager &
# gnome-power-manager &
gnome-screensaver &
eval `ssh-agent -s` & 
xset r rate 300 40 &
xmodmap ~/.Xmodmap &
# source ~/.bash_profile &
# `cat ~/.fehbg`
# feh --bg-tile ~/img/wallpapers/180.jpg
# feh --bg-scale ~/img/photos/2008/Library_-_136.jpg
feh --bg-scale ~/img/wallpapers/Patterns/diagonals.jpg
# exec gnome-session

# background 
# xsetroot -bg "#121212" -mod 3 3
#xsetroot -bg "#31659C" -mod 3 3
# xsetroot -solid gray20
xsetroot -solid gray35
xsetroot -cursor_name left_ptr

# xbindkeys
urxvt -name urxvt.wombat -e ssh-add ~/.ssh/id_rsa &
killall unclutter
unclutter -keystroke -idle 2 -regex -notclass Conkeror &
# start synergyd/c
# gnome-panel &
# autocutsel -f
~/src/gtk-clipboard-share-0.1/gtk-clipboard-share
# synergys
# sudo java -jar ~/code/blocked/blocked-1.0.0-SNAPSHOT-standalone.jar &
nm-applet --sm-disable &
dropbox start &
parcellite &
dunst &
# nautilus --no-default-window &
# cairo-compmgr &
#  > .swm-log 2>&1 
# sticky modifiers
#
# xkbset -bell -feedback sticky -twokey -latchlock
# xkbset exp 64 '=sticky' '=twokey' '=latchlock'
urxvtcd -e screen -S main
pulseaudio --start
xfce4-power-manager &
# sudo bash -c "echo level 2 > /proc/acpi/ibm/fan"
~/src/xflux/xflux -z 98837
xterm -e "screen -S stump stumpwm"
# xterm
# /usr/local/bin/stumpwm
