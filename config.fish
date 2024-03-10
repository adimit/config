# Config
# Created on 2016-02-09
# Author: Aleksandar Dimitrov <aleks.dimitrov@gmail.com>

# Vim on the command line
set -x EDITOR "/usr/bin/env vim"

# Emacs doesn't like the compose key to be AltGr and nobody cares
# See https://emacs.stackexchange.com/questions/3650/compose-key-in-emacs-multi-key-is-undefined/3910
set -x XMODIFIERS "@im=none"

alias ec "emacsclient -n"
alias ecc "emacsclient -cn"
alias ew "emacsclient -nw"
alias o open
alias l ls
alias ll 'ls -lh'
alias la 'ls -A'
alias grep "grep --color=auto"
alias egrep "egrep --color=auto"
alias rgrep "rgrep --color=auto"
alias rgr rgrep
alias mkdir "mkdir -p"
alias du "du -h"
alias df "df -h"
alias mpc "mpc --format '%position%: %artist% - %album% - %title%'"
alias ccp "rsync -rvau --info=progress2 --partial"
alias rm 'rm -iv'
alias mv 'mv -i'
alias cp 'cp -i'
alias icat 'kitty +kitten icat'
alias stand 'linak-controller --move-to stand'
alias sit 'linak-controller --move-to sit'

function ssh
    TERM=screen-256color command ssh $argv
end

for i in $HOME/local/*/bin
    set PATH $i $PATH
end

set PATH $HOME/.local/bin $PATH

if [ -d $HOME/.pub-cache/bin ]
    set PATH $HOME/.pub-cache/bin $PATH
end

if [ -d $HOME/.cargo/bin ]
    set -xg PATH $HOME/.cargo/bin $PATH
end

# Change into a directory and ls. Create it first, if it doesn't exist.
function c
    set -l folder $argv[1]
    if [ -d $folder ]
        cd $folder
        ls .
    else
        mkdir -p $folder
        cd $folder
        echo "Created $folder"
    end
end

# Remove directory mistakenly created by c
function unc
    set -l oldpwd (pwd)
    cd -
    rmdir $oldpwd
    echo "Removed $oldpwd"
end

function fish_user_key_bindings
    fish_vi_key_bindings
    bind -M insert \cf accept-autosuggestion
    bind \cf accept-autosuggestion
end

fish_vi_cursor
# Suppress default vi-mode indicator.
function fish_mode_prompt
end

set fish_color_command --bold blue

function fish_prompt
    # Last command exit code if non-zero
    set -l last_status $status
    if [ $last_status -ne 0 ]
        set_color --bold red
        printf "$last_status "
        set_color normal
    end

    # Number of background jobs
    set -l jobs (jobs | wc -l | tr -d '[:space:]')
    if [ $jobs -gt 0 ]
        set_color yellow
        printf "$jobs "
        set_color normal
    end

    # if logged in via SSH, show server
    if set -q SSH_CLIENT
        set_color blue
        echo -n (hostnamectl hostname)' '
        set_color normal
    end

    # Current git branch (simplified)
    set -l git_branch ( git branch 2> /dev/null \
                      | sed -n -e 's/\* \(.*\)/\1/p' \
                               -e 's/(detached from \(.*\))/d:\1/p' \
                               -e 's/(HEAD detached \(at\|from\) \(.*\))/d:\2/p' \
                               -e 's/^hotfix\//h\//p' \
                               -e 's/^feature\//f\//p' \
                      | tail -n 1 | sed -e 's/\(.\{19\}\).\+/\1…/')
    if set -q $git_branch
    else
        set_color --bold $fish_color_quote
        echo -n $git_branch' '
        set_color normal
    end

    set username (whoami)
    set default_color yellow
    set insert_color green
    set visual_color blue

    if [ $username = root ]
        set default_color red
        set insert_color red
        set visual_color red
    end

    # Prompt, indicating vi-mode
    switch $fish_bind_mode
        case default
            set_color --bold $default_color
            printf "● "
        case insert
            set_color --bold $insert_color
            printf "▶ "
        case visual
            set_color --bold $visual_color
            printf "■ "
    end
    set_color normal
end

function fish_right_prompt
    set_color blue
    echo -n (prompt_pwd)
    set_color normal
end

# Takes a floating ponit number and emits it in white, or yellow if it's >1.0 or
# red if it's >4.0
function render_load_average
    set -l load_average $argv[1]
    set_color normal
    if test $load_average -gt 5
        set_color --bold $fish_color_error
    else if test $load_average -gt 2
        set_color --bold yellow
    else if test $load_average -gt 1
        set_color --bold white
    end
    echo -n $load_average
    set_color normal
end

function fish_greeting
    set_color blue
    echo -n (date +"%k:%m %a, %b %d")
    set_color normal

    echo -n " | "
    set -l load_avg \
        ( uptime \
     | sed -e 's/\([0-9]\),\([0-9]\)/\1.\2/g' \
           -e 's/.*average: \([^,]\+\), \([^,]\+\), \([^,]\+\)/\1\n\2\n\3/'
     )

    echo (render_load_average $load_avg[1]) \
        (render_load_average $load_avg[2]) \
        (render_load_average $load_avg[3])
end

# Work around Java being a damn nitwit
set -x _JAVA_AWT_WM_NONREPARENTING 1

set local_config "$HOME/.config/fish/localconf.fish"
if [ -f $local_config ]
    source $local_config
end

set -x _JAVA_AWT_WM_NONREPARENTING 1

set -gx WASMTIME_HOME "$HOME/.wasmtime"

# Emacs LSP mode performance improvement
set -gx LSP_USE_PLISTS true

string match -r ".wasmtime" "$PATH" >/dev/null; or set -gx PATH "$WASMTIME_HOME/bin" $PATH

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
set -gx PATH $HOME/.cabal/bin $PATH /home/aleks/.ghcup/bin # ghcup-env

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH
