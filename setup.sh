# Create symlinks to all dotfiles in home directory
#

dotfiles="\
    .emacs.d \
    .vimrc \
    .zshrc \
    .tmux.conf \
"

for file in $dotfiles; do
    ln -s $PWD/$file $HOME/$file;
done
