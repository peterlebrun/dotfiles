# Create symlinks to all dotfiles in home directory

dotfiles="\
    .emacs.d \
    .vimrc \
    .zshrc \
    .tmux.conf \
"

for file in $dotfiles; do
    if [ -L $HOME/$file ]; then
       rm $HOME/$file; # Clean out old symlink
    fi
    ln -s $PWD/$file $HOME/$file;
done

# a little syntactic sugar
dotfiles_symlink=$HOME/eng/${PWD##*/}
if [ -L $dotfiles_symlink ]; then
    rm $dotfiles_symlink; # Clean out old symlink
fi
ln -s $PWD $dotfiles_symlink;
