# .bash_profile is invoked only for login shells
# except on OS X, where all shells are "login shells"

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

export RSTUDIO_WHICH_R=/usr/bin/R
