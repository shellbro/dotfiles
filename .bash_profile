# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs

PATH=$PATH:$HOME/.local/bin:$HOME/bin
export PATH

if [ -f /usr/local/bin/emacs ]; then
    EDITOR=/usr/local/bin/emacs # the latest release if available
else
    EDITOR=emacs # repository version taking into account user PATH
fi
export EDITOR
