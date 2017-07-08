PS1="[`whoami`@\h \W]$ "; export PS1

if [ -f ~/.bash_aliases ]; then
    source ~/.bash_aliases
fi
