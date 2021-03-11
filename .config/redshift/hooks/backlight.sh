#!/bin/sh
# Set brightness via xbrightness when redshift status changes

echo "$(date +%F)" $@ >> /tmp/redshift_hook

# Set brightness values for each status.
# Range from 1 to 100 is valid

brightness_day=85
brightness_transition=40
brightness_evening=10
brightness_night=2

set_brightness() {
	kill $(pgrep -f '.*xbacklight.*')
	xbacklight -time 30000 -fps 10 -set $1 &
}

if [ "$1" = period-changed ]; then
	case $3 in
		night)
			current_epoch=$(date +%s)
			target_epoch=$(date -d "22:00" +%s)
			set_brightness $brightness_evening
			sleep $(( $target_epoch - $current_epoch ))
			set_brightness $brightness_night

			;;
		transition)
			set_brightness $brightness_transition
			;;
		daytime)
			set_brightness $brightness_day
			;;
	esac
fi
