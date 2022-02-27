# .bash_profile is invoked only for login shells
# except on OS X, where all shells are "login shells"

if [ -f ~/.bashrc ]; then source ~/.bashrc; fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/johann/opt/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/johann/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/johann/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/johann/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

export RSTUDIO_WHICH_R=/usr/bin/R
