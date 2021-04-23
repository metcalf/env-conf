* Configure SSH keys with access to github
* Run:
```
cd ~
mkdir code
git clone git@github.com:metcalf/env-conf.git
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
brew install ansible
ansible-playbook --ask-become-pass -i hosts playbook.yml --connection=local
```
* Remap caps lock to control (possible to script but kind of a PITA)
* Make sure all the annoying mission control hotkeys are disabled
* Reconfigure Terminal
  - Copy the "Pro" profile
  - Text: 100% background opacity, Source Code Pro 12pt, Anti alias
  - Window: Limit number of rows to 20k
  - Shell: When shell exits close if the shell exited cleanly
* Configure Dash hotkey
* Comment out the "Host *" section of .ssh/config and configure personal github:
```
Host github.com-personal
    IdentityFile ~/.ssh/id_rsa_agmetcalf
    IdentitiesOnly yes
    HostName github.com
    User git
    PreferredAuthentications publickey

Host github.com-stripe
	HostName github.com
	User git
    PreferredAuthentications publickey
	IdentityFile ~/.ssh/id_rsa_andrew@stripe.com
    IdentitiesOnly yes
```
