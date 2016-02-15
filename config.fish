# Config
# Created on 2016-02-09
# Author: Aleksandar Dimitrov <aleks.dimitrov@gmail.com>

alias ec "emacsclient -n"
alias o "xdg-open"
alias l  "ls"
alias ll 'ls -lh'
alias la 'ls -A'
alias grep "grep --color=auto"
alias egrep "egrep --color=auto"
alias rgrep "rgrep --color=auto"
alias rgr "rgrep"
alias mkdir "mkdir -p"
alias du "du -h"
alias df "df -h"
alias mpc "mpc --format '%position%: %artist% - %album% - %title%'"
alias ccp "rsync -rvau --info=progress2 --partial"
alias rm 'rm -iv'
alias mv 'mv -i'
alias cp 'cp -i'

fish_vi_mode
fish_vi_cursor
function fish_mode_prompt; end

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
    echo -n (hostname -f)' '
    set_color normal
  end

  # Current git branch (simplified)
  set -l git_branch ( git branch ^ /dev/null \
                    | grep '^\*' \
                    | cut -f 2 -d ' ' \
                    | sed 's/feature/f/' \
                    | sed 's/hotfix/h/'
                    )
  if set -q $git_branch
  else
    set_color --bold $fish_color_quote
    echo -n $git_branch' '
    set_color normal
  end

  # Prompt, indicating vi-mode
  switch $fish_bind_mode
    case default
      set_color --bold $fish_color_comment
      printf "● "
    case insert
      set_color --bold green
      printf "▶ "
    case visual
      set_color --bold blue
      printf "● "
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
  if math "$load_average > 5.0" > /dev/null
    set_color --bold $fish_color_error
  else if math "$load_average > 2.0" > /dev/null
    set_color --bold yellow
  else if math "$load_average > 1.0" > /dev/null
    set_color --bold white
  end
  echo -n $load_average
  set_color normal
end

function fish_greeting
  set_color blue
  echo -n (date +"%k:%m %a, %b %d")
  set_color normal

  set -l hamster_status \
    ( hamster-cli list \
    | sed 's/( \+)/(just started)/' \
    | sed -n 's/^ *\([0-9:]\+\)[ -]\+( *\([^)]\+\))[ |]\+\([^ ]\+\).*$/\1\n\2\n\3/p'
    )
  if set -q hamster_status[1]
    echo -n " | "
    set_color --bold green
    echo -n "$hamster_status[3] "
    set_color normal
    set_color green
    echo -n "($hamster_status[2])"
    set_color normal
  end

  echo -n " | "
  set -l load_avg \
    ( uptime \
    | sed 's/.*average: \([^,]\+\), \([^,]\+\), \([^,]\+\)/\1\n\2\n\3/'
    )
  echo (render_load_average $load_avg[1]) \
       (render_load_average $load_avg[2]) \
       (render_load_average $load_avg[3])
end

set local_config "$HOME/.config/fish/localconf.fish"
if [ -f $local_config ]
    source ~/.config/fish/localconf.fish
end
