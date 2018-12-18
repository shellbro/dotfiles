# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

export PS1='[\t \u@\h \W $?]\$ '

if [ -f /usr/local/bin/emacs ]; then
    EDITOR=/usr/local/bin/emacs # the latest release if available
else
    EDITOR=emacs # repository version taking into account user PATH
fi
export EDITOR

export ANSIBLE_NOCOWS=1

# User specific aliases and functions
alias ec='emacsclient -n'
