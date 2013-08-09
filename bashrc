# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


export MYPS='$(echo -n "${PWD/#$HOME/~}" | awk -F "/" '"'"'{if (length($0) > 14) { if (NF>4) print $1 "/" $2 "/.../" $(NF-1) "/" $NF; else if (NF>3) print $1 "/" $2 "/.../" $NF; else print $1 "/.../" $NF; } else print $0;}'"'"')'
if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[00;36m\]\w$(__git_ps1 ":\[\033[00;32m\]%s ")\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls -G --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alFs'
alias la='ls -A'
alias l='ls -CF'

# various other aliases
alias ..='cd ..'
alias russian_roulette="if [ $RANDOM -gte 10000 ] then; sudo rm -rf /; fi;"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
source ~/.git-completion.sh

function cdl() {
    builtin cd "$1"
    ls -lastp | grep -v \\.pyc$ | grep -v "\\./$" | grep -v " \\." | head -n15
    echo "..."
}

# By Evan Broder:
# I got tired of bundle exec'ing things, so the following will automatically prefix
# any gem-installed command with "bundle exec" if you're in a directory with a Gemfile:

_find_gemfile() {
    local dir
    dir="$(pwd)"
    while [ "$dir" != "/" ]; do
        if [ -e "${dir}/Gemfile" ]; then
            echo "${dir}/Gemfile"
            return 0
        fi
        dir="$(dirname "$dir")"
    done

    return 1
}

_maybe_bundleify() {
    if _find_gemfile >/dev/null; then
        command bundle exec "$@"
    else
        command "$@"
    fi
}

for cmd in $(ls ~/.rbenv/shims); do
    case $cmd in
        gem|irb|bundle) continue ;;
        *) eval "$cmd() { _maybe_bundleify $cmd \"\$@\"; }" ;;
    esac
done

# By Evan Broder:
# You'll probably also want the following alias for pry so that you
# can still run it even in repositories that don't include pry in their Gemfile:

pry() {
    local gemfile
    if gemfile="$(_find_gemfile)"; then
        if grep -q pry "${gemfile}.lock"; then
            command bundle exec pry "$@"
        else
            command pry -rbundler/setup "$@"
        fi
    else
        command pry "$@"
    fi
}

export PATH
export PYTHONPATH="/usr/local/src/google_appengine/lib/ipaddr":$PYTHONPATH
export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
export EDITOR='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -a ""'
export VBOX_USER_HOME=~/.VirtualBox
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
