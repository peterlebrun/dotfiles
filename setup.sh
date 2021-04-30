#!/bin/bash
# Create symlinks to all dotfiles in home directory

dotfiles="\
    .emacs.d \
    .vimrc \
    .zshrc \
    .tmux.conf \
    .sbclrc \
    .hushlogin \
"

vscode="$HOME/Library/Application Support/Code/User"

for file in $dotfiles; do
    if [ -L $HOME/$file ]; then
       rm $HOME/$file; # Clean out old symlink
    fi
    ln -s $PWD/$file $HOME/$file;
done

vscodedotfiles="\
    settings.json \
    keybindings.json \
"

for file in $vscodedotfiles; do
    if [ ! -L "$vscode"/$file ]; then
        ln -s $PWD/vscode/$file "$vscode"/$file;
    fi
done

if [ ! -L $HOME/.oh-my-zsh/themes/pl-custom.zsh-theme ]; then
  ln -s $PWD/pl-custom.zsh-theme $HOME/.oh-my-zsh/themes/pl-custom.zsh-theme
fi

if [ ! -L ~/.config/starship.toml ]; then
  ln -s $PWD/starship.toml ~/.config/starship.toml
fi

if [ ! -L ~/.config/fish ]; then
  ln -s $PWD/fish ~/.config/fish
fi

if [ ! -L ~/.config/omf ]; then
  ln -s $PWD/omf ~/.config/omf
fi

# Set up iterm2
# Specify the preferences directory
# Tell iTerm2 to use the custom preferences in the directory
defaults write com.googlecode.iterm2.plist PrefsCustomFolder -string "$DOTFILES/iterm2-profile"
defaults write com.googlecode.iterm2.plist LoadPrefsFromCustomFolder -bool true