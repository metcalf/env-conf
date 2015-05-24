* Configure SSH keys with access to github
* Copy `vars.yml.sample` to `vars.yml` and fill in variables.
* Run:
```
cd ~
mkdir code
git clone git@github.com:metcalf/env-conf.git
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
brew install ansible
ansible-playbook --ask-sudo-pass -i hosts playbook.yml --connection=local
```
* Remap caps lock to control (possible to script but kind of a PITA)
