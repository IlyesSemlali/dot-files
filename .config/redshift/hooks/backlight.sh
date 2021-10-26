#!/bin/bash
# Set brightness via xbrightness when redshift status changes

# Set brightness values for each status.
# Range from 1 to 100 is valid

brightness_day=90
brightness_transition=50
brightness_evening=30
brightness_night=20

set_brightness() {
	if ! [ -e /tmp/backlight.lock ]
	then
		echo changing backlight
		id -u > /tmp/backlight.lock
		pkill -f '.*xbacklight.*'
		xbacklight -time 20000 -fps 10 -set $1
		rm /tmp/backlight.lock
	fi
}

if [ "$1" == "period-changed" ]
then
	case $3 in
		night)
			current_epoch=$(date +%s)
			target_epoch=$(date -d "22:00" +%s)
			if [ $(( $target_epoch - $current_epoch )) -lt 18000 ]
			then
				set_brightness $brightness_evening
				sleep $(( $target_epoch - $current_epoch )) || true
				set_brightness $brightness_night
			else
				set_brightness $brightness_night
			fi

			;;
		transition)
			set_brightness $brightness_transition
			;;
		daytime)
			set_brightness $brightness_day
			;;
	esac
fi
