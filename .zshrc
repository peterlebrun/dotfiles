# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
DISABLE_AUTO_TITLE=true;
DISABLE_AUTO_UPDATE=true;
plugins=(
  zsh-autosuggestions
  zsh-syntax-highlighting
  evalcache
  zsh-defer
)

source $ZSH/oh-my-zsh.sh
autoload bashcompinit
bashcompinit

[ -f $HOME/eng/private.sh ] && source $HOME/eng/private.sh
export DOTFILES="$HOME/eng/github.com/peterlebrun/dotfiles"
alias dot="cd $DOTFILES"
alias resource="source ~/.zshrc"

bindkey '^[[Z' autosuggest-accept #shift-tab

[ -d /opt/homebrew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
[ -L ~/.config/starship.toml ] && eval "$(starship init zsh)"