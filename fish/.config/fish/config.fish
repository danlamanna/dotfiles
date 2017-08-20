set -gx PATH $HOME/.local/bin $HOME/bin $PATH

alias l="ls -Alh"
alias lr="l -tr"
alias ll="ls -lh"
alias llr="ll -tr"
alias rsynca="rsync -avhP --append"
alias splitsane="split --numeric-suffixes --additional-suffix=.part"
alias mkvirtualenv3="mkvirtualenv --python=(which python3)"
alias vup="vagrant up"
alias vprov="vagrant provision"
alias vdest="vagrant destroy -f"
alias vssh="vagrant ssh"
alias jn="jupyter notebook --no-browser"
alias jnb="jupyter notebook"

eval (python2.7 -m virtualfish compat_aliases auto_activation global_requirements projects)
