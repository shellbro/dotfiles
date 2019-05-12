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
alias cdb='cd ~/Git/bitbucket.org/shellbro'
alias cdg='cd ~/Git/github.com/shellbro'
alias df-pretty='df -Th -x tmpfs -x devtmpfs'
alias docker-cleanup='docker image prune --force'
alias docker-pull-all='docker images --format "{{.Repository}}:{{.Tag}}" | grep -Fv "<none>" | grep -v "^local/" | xargs -L 1 docker pull'
alias dropbox='docker exec -ti -e "LANG=en_US.UTF-8" dropbox /home/dropbox-user/bin/dropbox'
alias ec='emacsclient -n'
alias iftop-lte='sudo iftop -i wwp0s20f0u6'
alias iftop-wifi='sudo iftop -i wlp3s0'
alias is-interactive-shell='[[ $- == *i* ]] && echo "yes" || echo "no"'
alias is-login-shell='shopt -q login_shell && echo "yes" || echo "no"'
alias oc-whoami='oc whoami -c && oc whoami -t && oc whoami'
alias pia-de-berlin-up='nmcli con up "PIA - DE Berlin"'
alias pia-de-berlin-down='nmcli con down "PIA - DE Berlin"'
alias sudo='sudo '
alias work-g-up='nmcli con up "Work - G (PPTP)"'
alias work-g-down='nmcli con down "Work - G (PPTP)"'
alias work-k-up='nmcli con up "Work - K (L2TP with IPsec)"'
alias work-k-down='nmcli con down "Work - K (L2TP with IPsec)"'
