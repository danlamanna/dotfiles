# virtualenvwrapper stuff
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    export WORKON_HOME=~/.envs
    source /usr/local/bin/virtualenvwrapper.sh
fi

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

if [ -f ~/.env_specific_bashrc ]; then
    . ~/.env_specific_bashrc
fi

alias uu="sudo apt-fast update && sudo apt-fast upgrade -y"

# I don't play any games.
alias e="emacsclient"
alias ed="emacs"
alias nano="emacs"
alias vim="emacs"
alias vi="emacs"
alias emasc="emacs"

alias ls="ls --color"
alias s="ls"
alias b="cd $OLDPWD"

# cd to the root httpdocs folder
alias cdh="source ~/bin/cdh"

# Always recursive grep, this may come back to haunt me
alias grep="grep -R "

# Quick finding of files by grepping name
function ff() {
    find . | grep $1
}

# To make CLI scripts trigger xdebug
# xdebug dan - Sets idekey to dan
# xdebug - Sets idekey to nothing
function xdebug() {
    export XDEBUG_CONFIG="idekey=$1"
    echo $XDEBUG_CONFIG
}

# SVN Stuff!
alias up="svn up"
alias st="svn st"
alias sa="svn st | grep ^? | tr -d ' ' | sed 's/\?//' | xargs svn add"
alias sd="svn st | grep ^! | tr -d ' ' | sed 's/\!//' | xargs svn delete"

# Collective git diff of modified files
alias gd="git status -s | grep ^\ M | sed 's/\ M\ //' | xargs git diff"

alias mysql="mysql --pager='less -S'"

function tmpmv() {
   mv $1 _$1
}

function file_to_clipboard() {
    xclip -sel clip < $1
    echo "$1's contents are now on the clipboard.";
}

# Extract tar, gz, zip, etc
function extract()
{
     if [ -f $1 ] ; then
         case $1 in
             *.tar.bz2)   tar xvjf $1     ;;
             *.tar.gz)    tar xvzf $1     ;;
             *.bz2)       bunzip2 $1      ;;
             *.rar)       unrar x $1      ;;
             *.gz)        gunzip $1       ;;
             *.tar)       tar xvf $1      ;;
             *.tbz2)      tar xvjf $1     ;;
             *.tgz)       tar xvzf $1     ;;
             *.zip)       unzip $1        ;;
             *.Z)         uncompress $1   ;;
             *.7z)        7z x $1         ;;
             *)           echo "'$1' cannot be extracted via >extract<" ;;
         esac
     else
         echo "'$1' is not a valid file"
     fi
}

clear

export TERM;
TERM=xterm-256color

export EDITOR;
EDITOR=$(which emacs)

export VISUAL_EDITOR;
VISUAL_EDITOR=$(which emacs)

#export WORKON_HOME="$HOME/.virtualenvs"
#source $HOME/bin/virtualenvwrapper_bashrc


export PAGER=less
export LESS='-iMFXSx4R'



export PATH
PATH=$PATH:/usr/lib/jvm/java-6-openjdk/bin
