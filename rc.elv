use re
use platform
use str
use readline-binding

# prompt
eval (starship init elvish)

# path
# add path directories like this:
# set paths = [$@paths /new/path/dir]

# aliases
fn ll { |@a| ls -la $@a }
fn lll { |@a| ls -lapG | less -R }
fn mcd { |d| mkdir -p $d ; cd $d }
fn tree { |@a| e:tree -CF $@a }
fn less { |@a| e:less -FSRXc $@a }
fn mdless { |@a| mdcat --ansi $@a | less -R }
fn path { put $@paths | each $echo~ }

fn .. { cd ../ }
fn ... { cd ../../ }
fn .3 { cd ../../../ }
fn .4 { cd ../../../../ }
fn .5 { cd ../../../../../ }
fn .6 { cd ../../../../../../ }
# fn ~ { cd ~ }                         # doesn't work

# Completions

## git

# fn all-git-branches {
#     # Note: this assumes a recent version of git that supports the format
#     # string used.
#     git branch -a --format="%(refname:strip=2)" | re:awk {|0 1 @rest| put $1 }
# }
# 
# var common-git-commands = [
#   add branch checkout clone commit diff init log merge
#   pull push rebase reset revert show stash status
# ]
# 
# set edit:completion:arg-completer[git] = {|@args|
#     var n = (count $args)
#     if (== $n 2) {
#         put $@common-git-commands
#     } elif (>= $n 3) {
#         all-git-branches
#     }
# }
# 
# # apt
# 
# if (!=s $platform:os "darwin") {
#     var all-packages = [(apt-cache search '' | re:awk {|0 1 @rest| put $1 })]
# 
#     set edit:completion:arg-completer[apt] = {|@args|
#         var n = (count $args)
#         if (== $n 2) {
#             # apt x<Tab> -- complete a subcommand name
#             put install uninstall
#         } elif (== $n 3) {
#             put $@all-packages
#         }
#     }
# }
# 
# set edit:completion:arg-completer[kill] = {|@args|
#     var n = (count $args)
#     if (== $n 2) {
#         put HUP INT QUIT ILL TRAP ABRT EMT FPE KILL BUS SEGV SYS PIPE ALRM TERM URG^
#         STOP TSTP CONT CHLD TTIN TTOU IO XCPU XFSZ VTALRM PROF WINCH INFO USR1 USR2
#     }
#     if (== $n 3) {
#         # lists PIDs and commands:
#         # ps | cut -c 1-5,25- |tail -n +2
#     }
# }

## Carapace

set-env CARAPACE_BRIDGES 'zsh,fish,bash,inshellisense' # optional
eval (carapace _carapace|slurp)
