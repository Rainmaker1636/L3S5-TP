Rappel : # pour commenter dans la bashrc

'''
# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'


#Dossier
alias ael='cd ~/L3S5-TP/AEL'

## L2
#alias pdc='cd ~/Documents/l2s4/pdc'
#alias math='cd ~/Documents/l2s4/math'
#alias oo='cd ~/Documents/l2s4/oo'
#alias tw='cd ~/Documents/l2s4/tw2'
#alias perso='cd ~/Documents/l2s4/Perso'
#alias asd='cd ~/Documents/l2s4/asd'
alias www='cd /var/www/html'

##L3
alias coo='cd ~/COO/'

#Logiciel
alias ca='ledit ocaml'
alias exaql='export CLASSPATH=$CLASSHPATH:aql.jar'
alias raboutique='rlwrap java edu.gsu.cs.ra.RA boutique'

#Proxy
alias gex='export https_proxy="cache-etu.univ-lille1.fr:3128"'
alias gexdis='export https_proxy=""'

#Git
alias ga='git add'
alias gco='git commit -m'
alias gll='git pull'
alias gsh='git push'
alias gst='git status'
alias gck='git checkout'

#Varible environnement
export EDITOR=gedit
export BROWSER=firefox
export PAGER=most
'''
