#!/bin/zsh

BG='#0a0c0f'
FG='#aacccc'
DATEFMT="^bg($BG) %a, %b %d ^fg(white)%H:%M"
SLEEP=5
HLBAK="#3a5a5a"
BITMAPS="${HOME}/etc/dzen2"
export MPD_HOST="localhost"

fdate() {
	date +${DATEFMT}
}

fload() {
	print -n $(cut -d \  -f 1 < /proc/loadavg)
}

fplay() {
	MPC=$(mpc)
	if [ -n "$(echo $MPC|grep playing)" ]; then
		print "^i($BITMAPS/note.xbm) "
		export PLAYING="true"
	elif [ -n "$(echo $MPC|grep paused)" ]; then
		print "^i($BITMAPS/pause.xbm) "
	fi
}

fmusic() {
	TITLE=$(mpc --format '%title%' | line)
	ARTST=$(mpc --format '%artist%' | line)
	ALBUM=$(mpc --format '%album%' | line)
	print " Playing ^fg(white)$TITLE^fg()\n      by ^fg(white)$ARTST^fg()\n    from ^fg(white)$ALBUM^fg()"
}

fcputemp() {
	print -n ${(@)$(</proc/acpi/thermal_zone/TZ00/temperature)[2,3]}
}

while true; do
	DATE=$(fdate)
	TEMP=$(fcputemp)
	LOAD=$(fload)
	MUSC=$(fmusic)
	PLAY=$(fplay)

	print "$PLAY^bg($HLBAK) $LOAD $TEMP $DATE"
	if [ $PLAY ]; then
		print "$MUSC"
	else
		print "\n        Not playing anything.\n "
	fi

	sleep ${SLEEP}
done
