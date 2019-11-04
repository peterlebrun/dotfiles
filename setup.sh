# Create symlinks to all dotfiles in home directory

dotfiles="\
    .emacs.d \
    .vimrc \
    .zshrc \
    .tmux.conf \
    .sbclrc \
"

for file in $dotfiles; do
    if [ -L $HOME/$file ]; then
       rm $HOME/$file; # Clean out old symlink
    fi
    ln -s $PWD/$file $HOME/$file;
done
