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
urxvtd -q -o -f&
unclutter&

# Initialize x-environment
xsetroot -cursor_name left_ptr
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
	xcompmgr&
	#cairo-clock&
	DBGFILE="${HOME}/.xmonad/debug"
	set_background
	dbus-launch ssh-agent gpg-agent --default-cache-ttl 604800 \
	--max-cache-ttl 1209600 --daemon $HOME/.cabal/bin/xmonad > $DBGFILE&
	XMPID=$!
	xscreensaver&
	wait $XMPID
}

start_gnome() {
	ssh-agent gnome-session
}

start_kde() {
	startkde
}


case $2 in
	xmonad) start_xmonad
	;;
	gnome) start_gnome
	;;
	kde4) start_kde
	;;
	dwm) start_dwm
	;;
esac
