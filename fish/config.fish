if [ -f $HOME/private.sh ];
    source $HOME/private.sh
end

set PATH $PATH /usr/local/opt/icu4c/bin
set PATH $PATH /usr/local/opt/icu4c/sbin
set -Ux DOTFILES $HOME/eng/github.com/peterlebrun/dotfiles
set -U fish_greeting ""
alias dotfiles="cd $DOTFILES"

function fish_right_prompt
  #intentionally left blank
end

starship init fish | source