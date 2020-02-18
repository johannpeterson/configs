#!/bin/zsh
#   ------------------------------------------------
#   .zshrc
#   johann peterson
#   2/16/2020
#   ------------------------------------------------

echo "executing $ZSH_ARGZERO"

if [[ $OSTYPE == *linux* ]]; then
    echo "os: Linux ($OSTYPE)"
    is_linux=yes
elif [[ $OSTYPE == *darwin* ]]; then
    echo "os: OX-X ($OSTYPE)"
    is_osx=yes
else
    echo "os: unknown ($OSTYPE)"
fi

#   ------------------------------------------------
#   PATH settings
#   ------------------------------------------------
# PATH="$HOME/Library/Haskell/bin":$PATH
PATH="$HOME/bin":"/usr/local/bin":"/opt/local/bin":"/opt/local/sbin":$PATH
PATH="/Users/johann/Library/Python/3.7/bin":$PATH

# Setting PATH for Python 3.7
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:${PATH}"
export PATH

# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH

# The orginal version is saved in .bash_profile.pysave
# PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export VIM_APP_DIR="/Applications/Development/"

export PATH

# PATH=$PATH:"$HOME/Library/Haskell/bin"
# PATH=$PATH:"/usr/local/Cellar/vim/7.4.488/bin"
# PATH=$PATH:"$HOME/Dropbox/todo"
# # MacPorts Installer addition on 2014-03-28_at_23:08:07: adding an appropriate PATH variable for use with MacPorts.
# PATH="/opt/local/bin":$PATH
# PATH="/opt/local/sbin":$PATH
# # Finished adapting your PATH environment variable for use with MacPorts.
# PATH="$HOME/bin":$PATH

#   ------------------------------------------------
#   Miscellaneous options
#   ------------------------------------------------

export HISTSIZE=2000
setopt APPEND_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE

if [ "$is_osx" ]; then
    export EDITOR=/usr/local/bin/vim
else
    export EDITOR=/usr/local/vim
fi

# default blocksize for ls, df, du
# http://natelandau.com/my-mac-osx-bash_profile/
export BLOCKSIZE=1k

export CLICOLOR_FORCE=TRUE
export CLICOLOR=1

test -e ~/.dircolors && \
    eval `dircolors -b ~/.dircolors`

# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

export GREP_OPTIONS='--color=auto'
export LESSOPEN="| source-highlight -f esc-solarized --style-file=esc-solarized.style -i %s -o STDOUT"
export LESS=" -R"

if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
