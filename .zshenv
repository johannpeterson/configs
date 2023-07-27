#   ------------------------------------------------
#   PATH settings
#   ------------------------------------------------

# PATH=$HOME/bin
# PATH+=/usr/local/bin
# PATH+=/opt/local/bin
# PATH+=/opt/local/sbin

    # ":/usr/bin" \
    # ":/bin" \
    # ":/usr/sbin" \
    # ":/sbin" \
    # ":/Library/TeX/texbin" \
    # ":/opt/X11/bin" \
    # ":/Applications/Postgres.app/Contents/Versions/latest/bin" \
    # ":/Users/johann/Library/Python/3.7/bin" \
    # ":/Library/Frameworks/Python.framework/Versions/3.7/bin"
export PATH=$PATH":$HOME/.local/bin"

# I could not get the directory with a space (/Applications/Racket 8.9/bin)
# to work in the PATH.
export PATH="$PATH:/Applications/Racket/bin"

export VIM_APP_DIR="/Applications/Development/"
