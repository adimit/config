# Fancy new zsh!
#
# Thu Sep 27 02:00:44 CEST 2007
#
# Aleksandar Dimitrov

set -o vi

localpath=$HOME/local
if [ -d $localpath ]; then
	for i in $(ls -1 $localpath); do
		basepath=$localpath/$i
		if [ -d $basepath/bin ]; then
			PATH=$basepath/bin:$PATH
		fi
		if [ -d $basepath/share/man ]; then
			MANPATH=$basepath/share/man:$MANPATH
		fi
	done
fi

cabalpath=${HOME}/.cabal/bin
if [ -d $cabalpath ]; then
	export PATH=$cabalpath:$PATH
fi

if [ -d $HOME/bin ]; then
	export PATH=$HOME/bin:$PATH
fi

### Environment

export VISUAL=/usr/bin/vim
export EDITOR=/usr/bin/vim
HISTFILE=~/.history
HISTSIZE=4000
SAVEHIST=2000
ECLIPSE_HOME=${HOME}/local/eclipse
export TEXMFHOME=${HOME}/.texmf
DIRSTACKSIZE=10

setopt histlexwords
setopt incappendhistory
setopt dvorak
setopt correct
setopt extendedhistory
setopt extended_glob
setopt histignoredups
setopt histallowclobber
setopt histignorespace
setopt autopushd
setopt noclobber
setopt autocd

### Aliases
if [ $(uname -s) = "Linux" ]; then
	alias ls='ls --color="auto" -CFB'
else
	alias ls='ls -GFBC'
fi
### Simple command aliases
alias ec='emacsclient'
alias wee='weechat-curses'
alias ll='ls -lh' la='ls -A' lsd='ls -d' l='ls'
alias vi=vim
alias grep='grep --color="auto"'
alias mkdir='nocorrect mkdir -p' touch 'nocorrect touch'
alias du='du -h' df='df -h'
alias iwscan="/sbin/iwlist wlan0 scan"
alias vim='vim -p'
alias gvim='gvim  -p'
alias scp='scp -r'
alias mpc="mpc --format '%artist% - %album% - %title%'"
alias ccp="rsync -rvrPua"
alias pls='pl -s'
alias nt='urxvt& disown %1'
alias p='mpc toggle'
alias o='fork mimeopen'
alias mo='mimeopen'
alias no='ls'
alias on='sl'

alias rm='rm -iv'
alias mv='mv -i'
alias cp='cp -i'

alias cpu='ps aux | sort -k 3,3 | tail '
alias mem='ps aux | sort -k 4,4 | tail '

alias cdrecord='sudo cdrecord driveropts=burnfree --verbose dev=/dev/sr0'
alias pss='ps -ef | grep $1'

## Directory hashes
if [ "$(hostname -s)" = "minsk" ]; then
	hash -d music=/media/music
else
	hash -d music=~/Music
fi

hash -d src=~/src
hash -d doc=~/doc
hash -d werti=~src/werti-passives
hash -d ws=~src/workspace
hash -d uni=~doc/uni

autoload -U compinit && compinit
zstyle ':completion:*' menu select=10
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -e -o pid,user,tty,cmd'

autoload -U zmv

### Keybindings
###############

bindkey '^p' history-beginning-search-backward
bindkey '^n' history-beginning-search-forward

### Functions

## Autoload
autoload -U zmv

fork() { (setsid "$@" &); }

# Move torrents to server
mvt() {
	TMPDIR=$HOME/var/tmp
	WATCHDIR=watch
	SERVER=bacon
	ssh-add -l > /dev/null || ssh-add
	ccp $TMPDIR/*.torrent $SERVER:$WATCHDIR \
		&& rm -f $TMPDIR/*.torrent \
		&& ssh $SERVER df -h
}

# create binary directories for opt
# useful for commercial packages like gearth, gwt, firefox and others  that don't come
# with a /bin in their release but rather put the binaries directly into their root
mkbin() {
	mkdir -p ./bin;
	cd ./bin;
	find ../ -maxdepth 1 -executable -type f -exec 'ln' '-s' '{}' ';'
	cd ..
}

# move stuff to a directory and c there
m() {
	mv $@
	if [ $? -eq 0 ] # -a -d $@[-1] ]
	then
		if [ -d $@[-1] ];
		then 
			c $@[-1]
		fi
	fi
}

# cd to a directory and create it, when neccessary
# and list what's in it
c() {
	if [ ! $1 = "-" -a ! -d $1 ]; then
		print "cdp: Creating $1"
		mkdir -p $1
	fi
	cd $@
	if [ $? -eq 0 ]; then ls; fi
}

hvim_session_name() { echo "hvim$1" }
# Usage:
# 	hvim # Attach to the latest hvim session
# 	hvim FILE # Create a new hvim session and open FILE
# 	hvim -t SESS # Attach to session SESS
hvim() {
	next_session=0;
	while [[ $? -eq 0 ]]; do
		tmux has-session -t $(hvim_session_name $next_session) 2&>> /dev/null \
			&& ((next_session=next_session+1))
	done
	if [[ -z $1 ]]; then # no file was given, attach to latest session
		((current_session=next_session-1))
		if [[ $current_session -eq -1 ]]; then
			echo "hvim: FATAL: no session to attach to, but no file given."
			return 1;
		else
			sess=$(hvim_session_name $current_session)
			tmux attach -t $sess
		fi
	elif [[ $1 == "-t" ]]; then # attach to specific session
		if [[ -z $2 ]]; then
			echo "hvim: FATAL: no session given with -t."
			return 2
		else
			sess=$(hvim_session_name $2)
			tmux attach -t $sess
		fi
	else # we're opening a new session
		sess=$(hvim_session_name $next_session)
		tmux -f ~/config/hvim.conf \
			new-session -s $sess -n vim -d "vim $1"
		tmux split-window -v -t $sess
		tmux send-keys -t $sess "ghci $1" C-m
		tmux select-pane -t $sess -l
		tmux resize-pane -t $sess -D 20
		tmux attach -t $sess
	fi
}

# Vim addiction
vimIsAwesome() { print "You're not in vim!"; }
:w() { vimIsAwesome; }
:wq() { vimIsAwesome; }
:q() { vimIsAwesome; }

# Automatic Recompilation

autrecomp() {
	file=$1; shift
	command="tset"
	if [ $1 ]; then
		export command=$1; shift
	fi
	while sleep_until_modified.py $file || sleep 1; do $command $@ $file; done
}

### Cosmetic stuff
# change title to current directory
if [ $TERM = "rxvt-unicode" ]; then
	TITLE="\e]0;%n@%m: %~\a" 
	print -Pn $TITLE # also initialize it
	precmd() { print -Pn $TITLE}
fi

# change title to current job
if [ $TERM = "rxvt-unicode" ]; then
	preexec() { print -Pn "\e]0;${1//\\/\\\\}\a" }
fi

## Run the curt system more comfortably:
curt() {
	pl -g curt -s $1
}

# Make a directory that will be tracked, but its content ignored by git
mkgitigndir () {
	mkdir $1
	echo "*" >| $1/.gitignore
	echo "!.gitignore" >> $1/.gitignore
}

autoload -U zmv

tset() {
	texdir=$(mktemp -d)
	texrun=$(which pdflatex)
	if [[ $1 == "-x" ]]; then texrun=$(which xelatex) fi
	if [ -x $texrun ]; then
		if [ ! -d $texdir ]; then
			mkdir $texdir
		fi
		$texrun -interaction nonstopmode -output-directory $texdir $@ \
			&& $texrun -output-directory $texdir $@ \
			&& mv -f $texdir/*{pdf,aux} .
	else
		echo "Could not find pdflatex/xetex executable: $texrun"
	fi
	rm -rf $texdir &> /dev/null
}

updong() {
	/usr/bin/uptime | perl -ne "/(\d+) d/;print 8,q(=)x\$1,\"D\n\""
}

disco() {
	mpc update --wait
	mpc clear
	cd ~music
	find -mindepth 2 -type d -mtime $1 -print | while read FNAME; do mpc add "$FNAME[3,-1]"; done
	mpc play
	mpc playlist
	cd -
}

jukebox() {
	player="/media/CLIP CLAP/MUSIC/"
	if [[ ! -d $player ]]; then
		echo "$player not mounted!"
		return
	fi
	cd /media/data/download
	find -mindepth 1 -maxdepth 1 -type d -mtime $1 -print | while read FNAME; do \
		flacs=$(ls "$FNAME/"*flac 2&>> /dev/null); \
		if [ $flacs ]; then copymusic $FNAME; fi; done
}

### Cache
zstyle ':completion::complete:*' use-cache 1

if [[ -n $SSH_CLIENT ]]; then
	PROMPTHOST="%m "
fi

## Prompt
autoload -U colors
colors

color() { echo "%{${fg[$1]}%}" }
CLDF="$(color 'default')"

maybe_hostname="$(color 'blue')${PROMPTHOST:-}$CLDF"
maybe_backgroundprocess="%1(j.$(color 'yellow')%j$CLDF .)"
maybe_errorcode="%(?..%B$(color 'red')%?%b$CLDF )"
user_prompt="%(#.%B$(color 'red').$(color green))%#%b"

PROMPTCHAR="$(color 'cyan')>$CLDF"
NORMALCHAR="$(color 'yellow')|$CLDF"

export SPROMPT="zsh: correct $(color 'red')%B%R%b$CLDF to $(color 'green')%B%r%b$CLDF [nyae]?"

TEMPPS1="\
$maybe_hostname\
$maybe_backgroundprocess\
$maybe_errorcode\
$user_prompt\
$CLDF" #switch to default color for rest of line.

PS1="$TEMPPS1$PROMPTCHAR "

function zle-line-init zle-keymap-select {
    PS1="$TEMPPS1${${KEYMAP/vicmd/$NORMALCHAR}/(main|viins)/$PROMPTCHAR} "
    PS2=$PS1
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

autoload -U edit-command-line
zle -N edit-command-line
bindkey '\ee' edit-command-line

RPS1="$(color blue)%~$CLDF"

localfile=$HOME/.zshrc.local
if [ -f $localfile ]; then
	source $localfile
fi

export GPGKEY=290BBA85

if [ $TOPICSTART ]; then
	ls
fi
