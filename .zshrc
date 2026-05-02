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
#   PATH settings - moved to .zshenv
#   ------------------------------------------------

# # PATH="$HOME/Library/Haskell/bin":$PATH

# # PATH=$PATH:"$HOME/Library/Haskell/bin"
# # PATH=$PATH:"/usr/local/Cellar/vim/7.4.488/bin"
# # PATH=$PATH:"$HOME/Dropbox/todo"
# # # MacPorts Installer addition on 2014-03-28_at_23:08:07: adding an appropriate PATH variable for use with MacPorts.
# # PATH="/opt/local/bin":$PATH
# # PATH="/opt/local/sbin":$PATH
# # # Finished adapting your PATH environment variable for use with MacPorts.
# # PATH="$HOME/bin":$PATH

# PATH="$HOME/bin":"/usr/local/bin":"/opt/local/bin":"/opt/local/sbin":$PATH
# PATH="/Users/johann/Library/Python/3.7/bin":$PATH

# # Setting PATH for Python 3.7
# # The original version is saved in .bash_profile.pysave
# PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:${PATH}"

# # Setting PATH for Python 3.6
# # The original version is saved in .bash_profile.pysave
# PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
# export PATH

# # The orginal version is saved in .bash_profile.pysave
# # PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
# export VIM_APP_DIR="/Applications/Development/"

#   ------------------------------------------------
#   Miscellaneous options
#   ------------------------------------------------

autoload -U compinit
compinit

export HISTSIZE=2000
setopt APPEND_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt AUTOPUSHD
unsetopt pathdirs

if (( ${+commands[fresh]} )); then
    export EDITOR='fresh'
    echo "fresh editor installed - setting as default"
else
    export EDITOR='emacs -nw'
    "setting 'emacs -nw' as default editor"
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

# export GREP_OPTIONS='--color=auto'
# export LESSOPEN="| source-highlight -f esc-solarized --style-file=esc-solarized.style -i %s -o STDOUT"
# export LESSOPEN="| src-hilite-lesspipe.sh %s"
# This uses the revised lessipipe script: https://gist.github.com/jaygooby/9494858d3d481a64819d227a9318f6c7
export LESSOPEN="| src-hilite-lesspipe.sh %s"
export LESS=" -R"

if [ -f ~/.aliases ]; then
    . ~/.aliases
    echo "Sourcing .aliases"
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

#   ------------------------------------------------
#   Prompt
#   ------------------------------------------------

if [ -f ~/.zshprompt ]; then
    . ~/.zshprompt
else
    PROMPT='%2~ %(!.#.>) '
fi


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/johann/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/johann/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/johann/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/johann/google-cloud-sdk/completion.zsh.inc'; fi

[ -f "/Users/johann/.ghcup/env" ] && source "/Users/johann/.ghcup/env" # ghcup-env
# The following lines have been added by Docker Desktop to enable Docker CLI completions.
fpath=(/Users/johann/.docker/completions $fpath)
autoload -Uz compinit
compinit
# End of Docker CLI completions

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

path=('/Users/johann/.juliaup/bin' $path)
export PATH

# <<< juliaup initialize <<<

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/johann/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/johann/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/johann/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/johann/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# >>> Added by Spyder >>>
alias uninstall-spyder=/Users/johann/Library/spyder-6/uninstall-spyder.sh
# <<< Added by Spyder <<<
