# Blurt: Created from CRUNCH (which is based off Dallas).
# CRUNCH - created from Steve Eley's cat waxing.
#
# This theme assumes you do most of your oh-my-zsh'ed "colorful" work at a single machine, 
# and eschews the standard space-consuming user and hostname info.  Instead, only the 
# things that vary in my own workflow are shown:
#
# * The time (not the date)
# * The current directory
# * The Git branch and its 'dirty' state
# 
# Colors are at the top so you can mess with those separately if you like.

if [ -f $HOME/private.sh ]; then
    source $HOME/private.sh
fi

BRACKET_COLOR="%{$fg[white]%}"
TIME_COLOR="%{$fg[yellow]%}"
DIR_COLOR="%{$fg[blue]%}"
GIT_BRANCH_COLOR="%{$fg[green]%}"
GIT_CLEAN_COLOR="%{$fg[green]%}"
GIT_DIRTY_COLOR="%{$fg[red]%}"

# These Git variables are used by the oh-my-zsh git_prompt_info helper:
ZSH_THEME_GIT_PROMPT_PREFIX="$BRACKET_COLOR:$GIT_BRANCH_COLOR"
ZSH_THEME_GIT_PROMPT_SUFFIX=""
ZSH_THEME_GIT_PROMPT_CLEAN=" $GIT_CLEAN_COLOR✓"
ZSH_THEME_GIT_PROMPT_DIRTY=" $GIT_DIRTY_COLOR✗"

# Our elements:
TIME_="$TIME_COLOR\$(date +%Y-%m-%d\|%H:%M) %{$reset_color%}"
DIR_="$DIR_COLOR%~ "
PROMPT="$BRACKET_COLOR➭ "

# Abbreviate directory path when we're in the static directory
# (because it saves screen space)
function get_dir() {
    dir="${PWD/$HOME/~}"
    static="${STATIC/$HOME/~}"
    echo "${dir/$static/static}"
}

# Note that the newline is intentional
# because it forces a line break after last command output
PROMPT="$TIME_$DIR_COLOR\$(get_dir) \$(git_prompt_info)"'
'"$PROMPT%{$reset_color%}"
