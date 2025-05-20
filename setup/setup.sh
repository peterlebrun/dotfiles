#!/bin/bash
# Create symlinks to all dotfiles in home directory
dotfiles="\
    .emacs.d \
    .zshrc \
"

for file in $dotfiles; do
    if [ -L $HOME/$file ]; then
       rm $HOME/$file; # Clean out old symlink
    fi
    ln -s $HOME/eng/github.com/peterlebrun/dotfiles/$file $HOME/$file;
done

# Wath out for this use of $PWD. Needs to verify that it's running from the correct directory.
if [ ! -L ~/.config/starship.toml ]; then
  ln -s $PWD/starship.toml ~/.config/starship.toml
fi

# Set up iterm2
# Specify the preferences directory
# Tell iTerm2 to use the custom preferences in the directory
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "$DOTFILES/iterm2-profile"
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true

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

# install fira code
if [ ! -f $HOME/Library/Fonts/FiraCode-VF.ttf ]; then
    brew tap homebrew/cask-fonts
    brew install --cask font-fira-code
fi

source $HOME/eng/github.com/peterlebrun/dotfiles/setup/gh-setup.sh

zshplugins="\
    zsh-syntax-highlighting \
    zsh-autosuggestions
"
for plugin in $zshplugins; do
    if [ ! -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/$plugin ]; then
        git clone https://github.com/zsh-users/$plugin.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/$plugin;
    fi
done
git clone https://github.com/romkatv/zsh-defer.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-defer
git clone https://github.com/mroth/evalcache ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/evalcache

# only works after vscode has been installed
defaults write -g ApplePressAndHoldEnabled -bool false