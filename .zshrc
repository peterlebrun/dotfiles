# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME='bira'

# Default to tmux on open
#if [ "$TMUX" = "" ];
  # Either check for sessions and attach to an existing tmux session
  # Or open a new session
  # Don't run this
  # then tmux list-sessions &> /dev/null && tmux -2 attach || tmux -2;
#  then tmux -2;
#fi

#
DISABLE_AUTO_TITLE=true;

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to disable command auto-correction.
# DISABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

alias z='vim ~/.zshrc'
alias zs='. ~/.zshrc'
alias t='vim ~/.tmux.conf'
alias v='vim ~/.vimrc'

# Not used.  Left for reference
# Function to clear ctrlp cache if it exists, then pull/rebase
# This requires that you are in the highest level directory of each repo
# i.e. /php, /templates, /resources, etc.
function gpull() {
  # store current working directory in $d
  d=`printf "${PWD##*/}"`;
  # find: searches for all files named *$d.txt
  # grep -q . : returns true if it finds any characters, false otherwise
  # rm: remove the cache file
  # notice the use of the && as a conditional here
  find ~/cache/ctrlp/ -maxdepth 1 -name '*'$d'.txt' 2>/dev/null | grep -q . && rm ~/cache/ctrlp/*$d.txt
  git pull --rebase
}

# Not used.  Left for reference.
# In case you forget the syntax to delete a remote branch, this takes care of it for you
# It will delete the remote for the current branch
function clear_remote_branch() {
  git push origin :`git branch | awk -F " " '/^\*/ { print $2}'`
};

# Diff of most recent commit to a location specific to me
# File is named as follows: mmdd-directory-branch.diff
# For example 0101-php-master.diff
# Obviously if you run this multiple times in the same date it will overwrite previous ones
# Also if you write multiple commits to a branch you will only get the most recent
function get_diff() {
  dash="-"
  # Grab date & time - time is used so that most recent diff always shows up at the bottom of the file list
  a=`date +%Y%m%d-%H%M%S`;
  # Get the name of the current working directory - just the directory, not the path
  b=`printf "${PWD##*/}"`;
  # Get the name of the current git branch
  c=`git branch | awk -F " " '/^\*/ { print $2}'`
  git diff --full-index HEAD~1..HEAD > ~/dev/util/diffs/"$a$dash$b$dash$c".diff
};

# Not used.  Left for reference.
function deploy_to_staging() {
# Add whatever's there to the latest commit
#git add -u && git commit --amend --no-edit
# avoid issues with the existing remote branch
#clear_remote_branch
# get hash of last commit
rev=HASH:`git rev-parse HEAD`
echo $rev
branch=BRANCH:`git branch | awk -F " " '/^\*/ { print $2}'`
echo $branch
email=AUTHOR_EMAIL:user@address.com
echo $email
# get repo
repo=REPO:includes
echo $repo
user=username:TOKEN
echo $user
# pass in parameters as key:value
}

alias realsync='perl ~/dev/util/realsync/realsync'
alias sync-php='realsync ~/dev/code/php | tee -a ~/dev/util/logs/php.log'
alias sync-resources='realsync ~/dev/code/resources | tee -a ~/dev/util/logs/resources.log'
alias sync-templates='realsync ~/dev/code/templates | tee -a ~/dev/util/logs/templates.log'
alias sync-chatbot='realsync ~/dev/code/chatbot | tee -a ~/dev/util/logs/chatbot.log'

autoload bashcompinit
bashcompinit
eval "$(_WAYRUNNER_COMPLETE=source wayrunner)"

# For anyone else using this
# Obviously this method won't handle things like needing to commit current changes
# It's a function of convenience, obviously inspired by abieber's 2016-03-29 talk
function pull() {
  current_directory=`printf "${PWD}"`
  repo_to_update=`printf "${HOME}/dev/code/$1"`
  cd $repo_to_update
  echo "Updating ${repo_to_update}"
  git pull --rebase origin master
  cd $current_directory
}

