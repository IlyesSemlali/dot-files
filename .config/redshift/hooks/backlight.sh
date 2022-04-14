#!/bin/bash
# Set brightness via xbrightness when redshift status changes

# Set brightness values for each status.
# Range from 1 to 100 is valid

brightness_day=90
brightness_transition=50
brightness_evening=30
brightness_night=20

if [ "$1" == "period-changed" ]
then
	case $3 in
		night)

			current_epoch=$(date +%s)
			target_epoch=$(date -d "22:00" +%s)

			if [ $(( $target_epoch - $current_epoch )) -lt 18000 ]
			then
				backlight soft_transition $brightness_evening
				sleep $(( $target_epoch - $current_epoch )) || true
				backlight soft_transition $brightness_night
			else
				backlight soft_transition $brightness_night
			fi

			;;

		transition)
			backlight soft_transition $brightness_transition
			;;

		daytime)
			backlight soft_transition $brightness_day
			;;
	esac
fi
