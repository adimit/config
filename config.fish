# Config
  
alias ec "emacsclient -n"
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

# Prompt
function fish_prompt
       set -l last_status $status
       # printf "$last_status "
       if [ $last_status -ne 0 ]
           set_color --bold red
           printf "$last_status "
       end
       set last_status $status
       # printf '%s' (__fish_git_prompt)
       set_color green
       switch $fish_bind_mode
           case default
               set_color --bold yellow
               printf "▶ "
           case insert
               set_color --bold green
               printf "▷ "
           case visual
               set_color --bold brown
               printf "● "
       end
       set_color normal
end

source ~/.config/fish/localconf.fish