# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

PS1='\[\e[33m\]\t\[\e[m\] \[\e[32m\]\u@\h\[\e[m\]:\[\e[34m\]\w\[\e[m\] \[\e[31m\]$?\[\e[m\]\$ '
export PS1

if [ -f /usr/local/bin/emacs ]; then
    EDITOR=/usr/local/bin/emacs # the latest release if available
else
    EDITOR=emacs # repository version taking into account user PATH
fi
export EDITOR

export ANSIBLE_NOCOWS=1

# User specific aliases and functions
alias ec='emacsclient -n'
alias df-pretty='df -Th -x tmpfs -x devtmpfs'
alias dropbox='sudo docker exec -ti -e "LANG=en_US.UTF-8" dropbox /home/dropbox-user/bin/dropbox'
alias iftop-lte='sudo iftop -i wwp0s20f0u6'
alias iftop-wifi='sudo iftop -i wlp3s0'
alias cdb='cd ~/Git/bitbucket.org/private'
alias cdg='cd ~/Git/github.com/sources'
