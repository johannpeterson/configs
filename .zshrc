#!/bin/zsh
#   ------------------------------------------------
#   .zshrc
#   johann peterson
#   2/16/2020
#   ------------------------------------------------

echo "executing $BASH_SOURCE"

if [[ $OSTYPE == *linux* ]]; then
    echo "os: Linux"
    is_linux=yes
elif [[ $OSTYPE == *darwin* ]]; then
    is_osx=yes
else
    echo "os: unknown"
fi

#   ------------------------------------------------
#   PATH settings
#   ------------------------------------------------
# PATH="$HOME/Library/Haskell/bin":$PATH
PATH="$HOME/bin":"/usr/local/bin":"/opt/local/bin":"/opt/local/sbin":$PATH
PATH="/Users/johann/Library/Python/3.7/bin":$PATH
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

# If this is an xterm set the title to user@host:dir
# case "$TERM" in
# xterm*|rxvt*)
#     PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
#     ;;
# *)
#     ;;
# esac

if [ "$is_osx" ]; then
    export EDITOR=/usr/local/bin/vim
else
    export EDITOR=/usr/local/vim
fi

# default blocksize for ls, df, du
# http://natelandau.com/my-mac-osx-bash_profile/
export BLOCKSIZE=1k

#   ------------------------------------------------
#   Colors and bash prompt strings
#   Chosen for Solarized color theme
#   http://ethanschoonover.com/solarized
#
#   usage:
#       prompt [big|normal|small|old|test] [light|dark]
#
#   If light/dark is unspecified, default is bland color which shows up
#   OK in either theme (base1).
#   Defaule prompt string is NORMAL_PROMPT
#   ------------------------------------------------

export CLICOLOR_FORCE=TRUE
export CLICOLOR=1

test -e ~/.dircolors && \
    eval `dircolors -b ~/.dircolors`

# enable color support of ls
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# export LSCOLORS=ExFxCxDxBxegedabagacad
# export LS_COLORS=’di=1:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:*.rpm=90′
# export LSCOLORS=gxfxcxdxbxexexabagacad
# export LS_COLORS=gxfxcxdxbxexexabagacad

export GREP_OPTIONS='--color=auto'
export LESSOPEN="| source-highlight -f esc-solarized --style-file=esc-solarized.style -i %s -o STDOUT"
export LESS=" -R"

# Solarized colors in OS X Terminal
# Base03   =  8 # bright black
export ClrBase03='\033[1;30m'
# Base02   =  0 # black
export ClrBase02='\033[0;30m'
# Base01   = 10 # bright green
export ClrBase01='\033[1;32m'
# Base00   = 11 # bright yellow
export ClrBase00='\033[1;33m'
# Base0    = 12 # bright blue
export ClrBase0='\033[1;34m'
# Base1    = 14 # bright cyan
export ClrBase1='\033[1;36m'
# Base2    =  7 # white
export ClrBase2='\033[0;37m'
# Base3    = 14 # bright white
export ClrBase3='\033[1;37m'
# Yellow   =  3 # yellow
export ClrYellow='\033[0;33m'
# Orange   =  9 # bright red
export ClrOrange='\033[1;31m'
# Red      =  1 # red
export ClrRed='\033[0;31m'
# Mangenta =  5 # magenta
export ClrMangenta='\033[0;35m'
# Violet   = 13 # violet
export ClrViolet='\033[1;35m'
# Blue     =  4 # blue
export ClrBlue='\033[0;34m'
# Cyan     =  6 # cyan
export ClrCyan='\033[0;36m'
# Green    =  2 # green
export ClrGreen='\033[0;32m'

#   ------------------------------------------------
#   Useful shell aliases
#   some from http://natelandau.com/my-mac-osx-bash_profile/
#   ------------------------------------------------

alias t='todo.sh -ANt'
alias tree='tree -CF'
alias less='less -FSRXc'
alias cdl='cd -'
alias ..='cd ../'                           # Go back 1 directory level
alias ...='cd ../../'                       # Go back 2 directory levels
alias .3='cd ../../../'                     # Go back 3 directory levels
alias .4='cd ../../../../'                  # Go back 4 directory levels
alias .5='cd ../../../../../'               # Go back 5 directory levels
alias .6='cd ../../../../../../'            # Go back 6 directory levels
alias ~='cd ~'                              # ~:            Go Home
alias c='clear'                             # c:            Clear terminal display
alias which='type -all'                     # which:        Find executables
alias path='echo -e ${PATH//:/\\n}'         # path:         Echo all executable Paths
alias show_options='shopt'                  # Show_options: display bash options settings
alias fix_stty='stty sane'                  # fix_stty:     Restore terminal settings when screwed up
alias cic='set completion-ignore-case On'   # cic:          Make tab-completion case-insensitive
mcd () { mkdir -p "$1" && cd "$1"; }        # mcd:          Makes new Dir and jumps inside
alias djm='python3 manage.py'
title () { echo -ne "\033]2;$1\007"; }      # title:        Set bash window title
# alias srcgrep='grep -rinI --exclude=*.log'
srcgrep () { grep -inrI --exclude=*.log $1 ./; }
findfile () { find / -name "$1" 1>&1 2>/dev/null; }
    
# http://unix.stackexchange.com/questions/38072/how-can-i-save-the-last-command-to-a-file
lastcommand() {
    fc -ln "$1" "$1" | sed '1s/^[[:space:]]*//'
}

if [ "$is_osx" ]; then
    echo "OX-X definitions."
    alias DT='tee ~/Desktop/terminalOut.txt'    # DT:           Pipe content to file on MacOS Desktop
    alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
    alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
    alias hideDesktop='defaults write com.apple.finder CreateDesktop -bool false;killall Finder'
    alias showDesktop='defaults write com.apple.finder CreateDesktop -bool true;killall Finder'
    # OS X WiFi utility, e.e. airport --getinfo; airport scan
    alias airport="/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources/airport"
    alias f='open -a Finder ./'                 # f:            Opens current directory in MacOS Finder
    trash () { command mv "$@" ~/.Trash ; }     # trash:        Moves a file to the MacOS trash
    alias firefox="/Applications/Firefox.app/Contents/MacOS/firefox-bin"
    ql () { qlmanage -p "$*" >& /dev/null; }    # ql:           Opens any file in MacOS Quicklook Preview
    alias ls='ls -Gp'
    alias ll='ls -alpG'
    alias lll='ls -alpG | less -R'
    alias l.='ls -d .*'
    alias l='ls -CF'
    alias la='ls -A'
    alias dircolors=gdircolors
    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    alert () { osascript -e 'display notification "Command complete."' ; }
else
    alias ls='ls -Gp --color'
    alias ll='ls -alpG --color'
    alias lll='ls -alpG --color | less -R'
    alias l.='ls -d --color .*'
    alias l='ls -CF'
    alias la='ls -A'
    alias v='xclip -o'
    alias c='xclip -selection clipboard'

    # Add an "alert" alias for long running commands.  Use like so:
    #   sleep 10; alert
    alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
fi


if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# The orginal version is saved in .bash_profile.pysave
# PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export VIM_APP_DIR="/Applications/Development/"

if [[ -e .bashrc ]]; then
    echo "source ~/.bashrc here"
    # source ~/.bashrc
fi

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# Setting PATH for Python 3.7
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.7/bin:${PATH}"
export PATH

# Setting PATH for Python 3.6
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.6/bin:${PATH}"
export PATH
