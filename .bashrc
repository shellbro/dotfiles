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
alias cdb='cd ~/Git/bitbucket.org'
alias cdh='cd ~/Git/github.com'
alias cdl='cd ~/Git/gitlab.com'
alias df-pretty='df -Th -x tmpfs -x devtmpfs'
alias docker-cleanup='docker image prune --force'
alias docker-pull-all='docker images --format "{{.Repository}}:{{.Tag}}" | grep -Fv "<none>" | grep -v "^local/" | xargs -L 1 docker pull'
alias dropbox='docker exec -ti -e "LANG=en_US.UTF-8" dropbox /home/dropbox-user/bin/dropbox'
alias ec='emacsclient -n'
alias iftop-lte='sudo iftop -i wwp0s20f0u6'
alias iftop-wifi='sudo iftop -i wlp3s0'
alias is-interactive-shell='[[ $- == *i* ]] && echo "yes" || echo "no"'
alias is-login-shell='shopt -q login_shell && echo "yes" || echo "no"'
alias k=kubectl
complete -F __start_kubectl k
alias k-context='kubectl config get-contexts | grep -e ^CURRENT -e "^\*" | tr -s " " | cut -d " " -f 2,5 | column -t'
alias k-contexts='kubectl config get-contexts'
alias k-context-switch='kubectl config use-context'
alias k-ns-switch='kubectl config set-context --current --namespace'
alias lsblk-pretty='lsblk -o NAME,TYPE,FSTYPE,LABEL,SIZE,MOUNTPOINT'
alias oc-login='oc login https://localhost:8443 -u developer -p foo'
alias oc-whoami='oc whoami -c && oc whoami -t && oc whoami'
alias sudo='sudo '
alias work-k-up='nmcli con up "Work - K (PPTP)"'
alias work-k-down='nmcli con down "Work - K (PPTP)"'
alias work-k2-up='nmcli con up "Work - K (L2TP with IPsec)"'
alias work-k2-down='nmcli con down "Work - K (L2TP with IPsec)"'
