# Fancy new zsh!
#
# Thu Sep 27 02:00:44 CEST 2007
#
# Aleksandar Dimitrov

set -o vi

export PATH="${HOME}/bin:${PATH}"

### Environment

export VISUAL=/usr/bin/vim
export EDITOR=/usr/bin/vim
HISTFILE=~/.history
HISTSIZE=4000
SAVEHIST=2000
ECLIPSE_HOME=${HOME}/local/eclipse
export TEXMFHOME=${HOME}/.texmf
DIRSTACKSIZE=10

setopt incappendhistory
setopt dvorak
setopt correct
setopt extendedhistory
setopt histignoredups
setopt histallowclobber
setopt histignorespace
setopt autopushd
setopt noclobber
setopt autocd

### Aliases
if [ $(uname -s) = "Darwin" ]; then
	alias ls='ls -GFBC'
else
	alias ls='ls --color="auto" -CFB'
fi
### Simple command aliases
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
alias ccp="rsync -rvr --progress"
alias pls='pl -s'
alias nt='urxvt&'

alias rm='rm -iv'
alias mv='mv -i'
alias cp='cp -i'

alias cpu='ps aux | sort -k 3,3 | tail '
alias mem='ps aux | sort -k 4,4 | tail '

alias cdrecord='sudo cdrecord driveropts=burnfree --verbose dev=/dev/sr0'
alias pss='ps -ef | grep $1'

## Directory hashes
if [ "%m" = "minsk" ]; then
	hash -d music=/media/minsk.crypto/music
else
	hash -d music=~/Music
fi
hash -d src=~/Documents/src
hash -d werti=~/Documents/uni/werti

autoload -U compinit
compinit

### Keybindings
###############

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

### Functions

## Autoload
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
NORMALCHAR="$(color 'yellow')∙$CLDF"

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

source ${HOME}/.zshrc.local

export GPGKEY=11076BD2

if [ $TOPICSTART ]; then
	ls
fi
