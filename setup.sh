#!/bin/bash
set -e

: "${DOTFILES:=$HOME/eng/github.com/peterlebrun/dotfiles}"

# Create symlinks to all dotfiles in home directory
dotfiles="\
    .emacs.d \
    .zshrc \
    .wezterm.lua \
"

for file in $dotfiles; do
    if [ -e "$HOME/$file" ] || [ -L "$HOME/$file" ]; then
       rm -rf "$HOME/$file"
    fi
    ln -s "$DOTFILES/$file" "$HOME/$file"
done

mkdir -p ~/.config
if [ ! -L ~/.config/starship.toml ]; then
  ln -s "$DOTFILES/starship.toml" ~/.config/starship.toml
fi

# install oh-my-zsh if it doesn't exist
if [ ! -d ~/.oh-my-zsh ]; then
    sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)";
fi

# install homebrew
brew -v >/dev/null 2>&1 || /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)";
[ -d /opt/homebrew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
# install starship
starship -V >/dev/null 2>&1 || brew install starship
jq --version >/dev/null 2>&1 || brew install jq
tree --version >/dev/null 2>&1 || brew install tree
gpg --version >/dev/null 2>&1 || brew install gpg
aws --version >/dev/null 2>&1 || brew install awscli
fnm --version >/dev/null 2>&1 || brew install fnm
wezterm --version >/dev/null 2>&1 || brew install wezterm

zshplugins="\
    zsh-syntax-highlighting \
    zsh-autosuggestions
"
for plugin in $zshplugins; do
    if [ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/$plugin ]; then
        git clone https://github.com/zsh-users/$plugin.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/$plugin;
    fi
done
if [ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-defer ]; then
    git clone https://github.com/romkatv/zsh-defer.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-defer
fi
if [ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/evalcache ]; then
    git clone https://github.com/mroth/evalcache ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/evalcache
fi

# only works after vscode has been installed
defaults write -g ApplePressAndHoldEnabled -bool false