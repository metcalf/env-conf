# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

source $HOME/.rbenvrc

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

add_to_path() {
    [ -d "$1" ] && export PATH="$PATH:$1"
}

add_to_path ~/stripe/password-vault/bin
add_to_path ~/stripe/space-commander/bin
add_to_path ~/stripe/pay-server/scripts/bin
add_to_path ~/stripe/henson/bin
add_to_path ~/bin
add_to_path /usr/local/lib/hadoop/bin

### BEGIN HENSON
export PATH="/Users/andrew/stripe/henson/bin:$PATH"
### END HENSON
