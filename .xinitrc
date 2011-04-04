#!/bin/bash

# if no WM is set, default to xmonad
if [ -z $1 ]; then
	WM=xmonad
else
	WM=$1
fi

if [ "$WM" == "xmonad" ]; then
	# stuff I only need with xmonad
	kdeinit4 &
	klipper &
	urxvt &
	#Esetroot /home/corrupt/Wallpapers/xmonadwp.png
	Esetroot thetismoon2k723x.jpg
fi

akregator --hide-mainwindow &
/home/corrupt/.bin/tcmount_stuff &
firefox &
xmodmap ~/.Xmodmap &
exec $WM

# vim:ft=sh
