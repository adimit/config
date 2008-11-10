#!/bin/zsh
# Initialize common environment
BACKFILE="${HOME}/.backgrounds"
COLOR_BG="#11121a"
COLOR_FG="#349d58"
FONT="-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*"
export ECLIM_ECLIPSE_HOME="${HOME}/opt/ganymede"
export JAVA_HOME=/opt/jdk1.6.0_10


. ${HOME}/.environment

# Set Path
export PATH="$HOME/bin:${PATH}"
if [ -d ${HOME}/opt ]; then
	for i in ${HOME}/opt/*/bin; do
		PATH="$i:${PATH}"
	done
fi

# Start daemons
fetchmail -m "/usr/bin/procmail -d %T"
urxvtd -q -o -f&
unclutter&
pulseaudio -D &

# Initialize x-environment
setxkbmap -layout us,bg -variant dvp, -option "grp:menu_toggle" \
					 -option "grp_led:scroll" \
					 -option "compose:ralt"
xsetroot -cursor_name left_ptr
xmodmap ~/.Xmodmap
xset r rate 145 35

set_background() {
	RES=`xdpyinfo | grep dimensions  | cut -c 18-26`
	IMAGE=`grep $RES $BACKFILE | cut -d ':' -f 2`
	FLAGS=`grep $RES $BACKFILE | cut -d ':' -f 3`
	eval "feh $FLAGS $IMAGE"
}

start_dwm() {
	ssh-agent dwm&
}

start_xmonad() {
	set_background
	DBGFILE="${HOME}/.xmonad/debug"
	dbus-launch ssh-agent gpg-agent --default-cache-ttl 604800\
	  --max-cache-ttl 1209600 --daemon xmonad > $DBGFILE&
	XMPID=$!
	wait $XMPID
}

start_gnome() {
	ssh-agent gnome-session
}

start_kde() {
	kdeinit4
	kwin&
	plasma
}


case $1 in
	xmonad) start_xmonad
	;;
	gnome) start_gnome
	;;
	kde) start_kde
	;;
	dwm) start_dwm
	;;
	*) start_xmonad
	;;
esac
