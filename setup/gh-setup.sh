function gh_conditional_install() {
    # ensure that gh is installed
    gh --version >/dev/null 2>&1 && \
    # ensure that extension isn't already installed
    ! gh extension list | grep -q $1 && \
    # install extension
    gh extension install $1
}

# install github and some extensions
gh --version >/dev/null 2>&1 || brew install gh
fzf --version >/dev/null 2>&1 || brew install fzf
if gh auth status | grep "You are not logged in*"; then
    # this line will require interactivity
    gh auth login
fi
gh_conditional_install mislav/gh-branch
gh_conditional_install dlvhdr/gh-dash