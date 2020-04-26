# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature
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
alias c-prune='{ sudo docker container prune -f &&\
                 sudo podman container prune; }'
alias c='{ sudo docker ps -a && sudo podman ps -a; }'
alias cdbb='cd ~/Git/bitbucket.org'
alias cdgh='cd ~/Git/github.com'
alias cdgl='cd ~/Git/gitlab.com'
alias check-postfix='postqueue -p | grep -c "^[A-F0-9]"'
alias check-unbound='sudo unbound-control stats_noreset | grep total'
alias crontab-loop='sudo bash -c "cat /var/spool/cron/*"'
alias df-pretty='df -Th -x tmpfs -x devtmpfs'
alias docker='sudo docker'
alias dropbox='sudo docker exec -it -e "LANG=en_US.UTF-8" dropbox\
               /home/dropbox-user/bin/dropbox'
alias ec='emacsclient -n'
# find backup files, auto-save files, interlock symbolic links
alias emacs-files='sudo find / -name "*~" -o -name "#*#" -o -name ".#*"'
alias gcp-ssh='gcloud compute ssh'
complete -F _complete_alias gcp-ssh
alias git-config-gh='git config user.name "Jakub Gorczyca" &&\
                    git config user.email shellbro@users.noreply.github.com'
alias git-status='find ~/Git -mindepth 4 -maxdepth 4 -type d -exec bash -c\
                  "(echo '{}' && cd '{}' && git status && echo)" \;'
alias i-prune='{ sudo docker image prune -f && sudo podman image prune; }'
alias i-prune-all='{ sudo docker image prune -a -f &&\
                     sudo podman image prune -a; }'
alias i='{ sudo docker images --digests && sudo podman images --digests; }'
alias iftop-lte='sudo iftop -i wwp0s20f0u6'
alias iftop-wifi='sudo iftop -i wlp3s0'
alias ip-external='curl https://www.gorczyca.xyz/cgi-bin/ip'
alias ip-internal='hostname -I'
alias ip-stats-lte='ip -s -h link show wwp0s20f0u6'
alias ip-stats-wifi='ip -s -h link show wlp3s0'
alias is-interactive-shell='[[ $- == *i* ]] && echo "yes" || echo "no"'
alias is-login-shell='shopt -q login_shell && echo "yes" || echo "no"'
alias journalctl-errors='sudo journalctl -b -p 3'
alias journalctl='sudo journalctl'
alias k-logs='kubectl logs --since=1s -f'
complete -F _complete_alias k-logs
alias k-nodes='kubectl get pod\
               -o=custom-columns=NODE:.spec.nodeName,NAME:.metadata.name\
                 | (read -r; printf "%s\n" "$REPLY"; sort)'
alias k=kubectl
complete -F __start_kubectl k
alias kctx=kubectx
alias kernel-cleanup='sudo package-cleanup --oldkernels'
alias kns=kubens
alias ls.='ls -d .*'
alias ll.='ls -lhd .*'
alias ll='ls -lh'
alias lsblk-pretty='lsblk -o NAME,TYPE,FSTYPE,LABEL,SIZE,MOUNTPOINT'
alias m=minikube
complete -F __start_minikube m
alias speedtest='speedtest-cli'
alias systemctl-failed='SYSTEMD_COLORS=1 systemctl --state=failed | less -R'
alias sudo='sudo '
alias tree='tree -aF -I ".git"'
alias ts='date +%s'
alias update-history='sudo yum history'
alias update-verify-conf='sudo find /etc -name "*.rpmnew" -or -name "*.rpmsave"'
alias update-verify='sudo rpm -Va'
alias wallpaper-size='xdpyinfo | grep dimensions | cut -d " " -f 7'
alias watch='watch '
alias wg-up='sudo wg-quick up wg0'
alias wg-down='sudo wg-quick down wg0'
alias work-k-up='nmcli con up "Work - K (L2TP with IPsec)"'
alias work-k-down='nmcli con down "Work - K (L2TP with IPsec)"'
alias work-k2-up='nmcli con up "Work - K (PPTP)"'
alias work-k2-down='nmcli con down "Work - K (PPTP)"'

function benchmark {
  for run in {1..5}
  do
    sudo hdparm -Tt "$1"
    sleep 10
  done
}

function burp {
  java -jar ~/bin/burpsuite_community.jar &
  chromium-browser --incognito --proxy-server=127.0.0.1:8080 http://burp &
}

function cdls {
  cd "$@" && ls
}

function file-exts {
  find "$1" -type f | perl -ne 'print $1 if m/\.([^.\/]+)$/'\
    | sort -fu | tr '[:upper:]' '[:lower:]'
}

function media-rename {
  exiftool '-filename<CreateDate' -d 'Photo %Y-%m-%d %H:%M:%S%%-c.%%le' -r\
           -ext jpg -ext heic "$1" &&\
  exiftool '-filename<CreateDate' -d 'Video %Y-%m-%d %H:%M:%S%%-c.%%le' -r\
           -ext mpg -ext avi -ext mp4 -ext mov "$1"
}

function repl {
  sudo docker run --rm -it --detach-keys=ctrl-@\
       -v "$HOME/.m2:/home/app-user/.m2"\
       --entrypoint=lein\
       shellbro/devbox-clojure update-in :dependencies into "[$1]" --\
       repl
}

function ts2date {
  date -d "@$1"
}
