# Oh My Zsh Configuration
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

# Dotfiles Management
export DOTFILES="$HOME/eng/github.com/peterlebrun/dotfiles"
alias dot="cd $DOTFILES"
alias resource="source ~/.zshrc"

# Private Configuration
[ -f $HOME/eng/private.sh ] && source $HOME/eng/private.sh

# Key Bindings
bindkey '^[[Z' autosuggest-accept  # shift-tab


# Tool Initialization
[ -d /opt/homebrew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
[ -L ~/.config/starship.toml ] && eval "$(starship init zsh)"
eval "$(pyenv init -)"
eval "$(fnm env --use-on-cd --shell zsh)"