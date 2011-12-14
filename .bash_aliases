alias smartgit='~/smartgit/bin/smartgit.sh'
alias ffdev='firefox -no-remote -P development'
alias ntest='./manage.py test --settings=django_settings.settings_test_fast'

alias gac='git add . && git commit -a'
alias gacp='git add . && git commit -a && git push origin HEAD'
alias gpull='git pull origin'
alias gstaging='git push origin HEAD:staging'
alias gmaster='git push origin HEAD:master'

alias so='source ve/bin/activate'
alias ..='cd ..'
alias nssh='ssh -p 3001'
alias nscp='scp -P 3001'

alias ssh-apple='ssh c9w@ec2-184-73-145-140.compute-1.amazonaws.com'
alias ssh-banana='ssh c9w@ec2-75-101-216-133.compute-1.amazonaws.com'

alias knockall="cat /etc/hosts | grep '\.nimbus' | awk '{ print $2 }' | xargs -I %s knock %s 1003"