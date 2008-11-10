# Fancy new zsh!
#
# Thu Sep 27 02:00:44 CEST 2007
#
# Aleksandar Dimitrov
#

# Set keymap
if [ $TERM = "linux" ]; then
	loadkeys ~/.capsesc_swap&>/dev/null
fi

set -o vi
PATH="${HOME}/bin:${PATH}"

### Environment
###############

export VISUAL=/usr/bin/vim
export EDITOR=/usr/bin/vim
### History
HISTFILE=~/.history
HISTSIZE=4000
SAVEHIST=2000

ECLIPSE_HOME=${HOME}/opt/ganymede

# Tex
export TEXMFHOME=${HOME}/.texmf

### PUSHD History
DIRSTACKSIZE=10

# Paths
#
JAVADIR=${HOME}/opt/java
if [ -d $JAVADIR ]; then
	for i in $JAVADIR/*.jar; do
		CLASSPATH=$i:$CLASSPATH
	done
fi

### Mail

mailpref="${HOME}/mail"
mailpath=($mailpref/inbox/new'?You have mail'
	$mailpref/wolfgang/new'?Wolfgang sent mail'
	$mailpref/ISCL/new'?New ISCL mail arrived'
	$mailpref/fachschaft/new'?Fachschaft stirred'
	$mailpref/detmar/new'?New mail in llearn'
)


### OPTIONS
###########

## History
setopt incappendhistory
setopt dvorak
setopt correct
setopt extendedhistory
setopt histignoredups
setopt histallowclobber
setopt histignorespace
setopt autopushd

## Misc
setopt noclobber
setopt autocd

### Aliases
###########

# color for different kinds of documents
eval `dircolors`

### Simple command aliases
alias vi=vim
alias ls='ls --color="auto" -CFB' ll='ls -lh' la='ls -A' lsd='ls -d' l='ls'
alias mkdir='nocorrect mkdir -p' touch 'nocorrect touch'
alias du='du -h' df='df -h'
alias umerge='emerge -C'
alias iwscan="/sbin/iwlist eth1 scan"
alias grep='grep --color="auto"'
alias vim='vim -p'
alias gvim='gvim  -p'
alias scp='scp -r'
alias mpc="mpc --format '%artist% - %album% - %title%'"
alias ccp="rsync -rvr --progress"
alias pl='swipl'
alias pls='swipl -s'
alias nt='urxvtc'
alias gcal='gcalcli'
alias calm='gcalcli calm'

alias rm='rm -iv'
alias mv='mv -i'
alias cp='cp -i'

alias cpu='ps aux | sort -k 3,3 | tail '
alias mem='ps aux | sort -k 4,4 | tail '

alias cdrecord='sudo wodim driveropts=burnfree --verbose dev=/dev/hdb'

# pastebins
alias rafb='pastebinit -b http://rafb.net -a Mantaar -i'

alias news='newsbeuter'

# grep aliases
alias pss='ps -ef | grep $1'

### Suffix aliases
alias -s tex=gvim cpp=gvim java=gvim xml=gvim php=gvim
alias -s html=firefox
alias -s jpg=feh jpeg=feh png=feh gif=feh

## Directory hashes
hash -d wp=~/etc/wallpapers
hash -d music=~/music
hash -d src=~/src
hash -d werti=~/src/WERTi

# common typos
alias iv=vi

### Global aliases

### Completion
##############

autoload -U compinit
compinit

### Keybindings
###############

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

### Functions
#############

### Autoload
autoload -U zmv

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

# Vim addiction
:w() {
	print "You're not in vi!"
}

:wq() {
	print "You're not in vi!"
}

q() {
	print "You're not in vi!"
}

# copy all torrent files to my torrent server
copytorrents() {
	find ${HOME}/var/tmp -maxdepth  1 -name '*torrent' \
	-exec 'scp' '{}' 'payak:var/p2p' ';' -delete
}

# find a todo file somewhere down the fs hierarchy
find_todo() {
	while [ ! -f ./TODO ]; do
		if [ $(pwd) = "${HOME}" ]; then
			echo "You don't have a TODO file. Go make one up, you bum!" 1>&2
			echo "${HOME}/TODO"
			return
		fi
		cd ..
	done
	if [ -f ./TODO ]; then
		echo "$(pwd)/TODO"
	fi
}

# manage a todo file found by find_todo/0
todo() {
	TODO=$(find_todo)
	ACTION=cat
	for i in $@; do 
		case $i in
			'-e') ACTION="$EDITOR"
			;;
			'-h') TODO="$HOME/TODO"
			;;
			*) print "What?" 1>&2
			return;
			;;
		esac
	done
	eval "$ACTION $TODO"
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

zle-keymap-select() {
	local showcmd="[C]"
	local showins="[I]"
	if [[ $KEYMAP = vicmd ]]; then
		if [[ $PS1 != ${showcmd}* ]]; then
			if [[ $PS1 = ${showins}* ]]; then
				PS1=$showcmd${PS1##$showins}
			else
				PS1="$showcmd$PS1"
			fi
			zle reset-prompt
		fi
	else
		if [[ $PS1 != ${showins}* ]]; then
			if [[ $PS1 = ${showcmd}* ]]; then
				PS1=$showins${PS1##$showcmd}
			else
				PS1="$showins$PS1"
			fi
			zle reset-prompt
		fi
	fi
}
zle -N zle-keymap-select

## Run the curt system more comfortably:

curt() {
	swipl -g curt -s $1
}


### Cache
zstyle ':completion::complete:*' use-cache 1

source ${HOME}/.zshrc.local

### Workaround for tiling WMs and urxvt (doesn't work)
sleep 0.2 && kill -SIGWINCH $$

## Prompt
autoload -U promptinit
promptinit
PS1="%{${fg[green]}${bg[default]}%}%{${fg[cyan]}${bg[default]}%}%D{%H:%M}%(?.. %B%{${fg[red]}${bg[default]}%}%?%b)%{${bg[default]}${fg[green]}%} %# %{${fg[default]}${bg[default]}%}"
RPS1="%B%{${fg[blue]}${bg[default]}%}%~%{${fg[green]}${bg[default]}%}%{${fg[default]}${bg[default]}%}%b"
