# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

function nonzero_return() {
	RETVAL=$?
	[ $RETVAL -ne 0 ] && echo -e " \e[31m$RETVAL\e[m"
}
PS1="\t \[\e[32m\]\u@\h\[\e[m\]:\[\e[34m\]\w\[\e[m\]\`nonzero_return\`\$ "
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
