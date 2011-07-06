#!/bin/bash

# if no WM is set, default to xmonad
if [ -z $1 ]; then
	WM=xmonad
else
	WM=$1
fi

if [ "$WM" == "xmonad" ]; then
	# stuff I only need with xmonad
	Esetroot /home/corrupt/Downloads/Wood\ Wallpaper\ Dark/Wood\ Wallpaper_1600.jpg
	urxvt &
fi

xset +fp /usr/share/fonts/local
xset rehash

liferea &
/home/corrupt/.bin/tcmount_stuff &
firefox &
pidgin &
skype &
xmodmap ~/.Xmodmap &
exec $WM

# vim:ft=sh
