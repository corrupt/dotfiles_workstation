# Lines configured by zsh-newuser-install {{{
HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=100000
setopt autocd notify inc_append_history autolist automenu share_history hist_ignore_dups extendedglob correct
unsetopt beep equals

# }}}
# The following lines were added by compinstall {{{
zstyle :compinstall filename '/home/corrupt/.zshrc'

autoload -U compinit promptinit
compinit
promptinit; prompt walters

# }}}

# general {{{

#nice completion features including the fancy menu-mode
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion:*' menu select=2
# case-insensitive (all) completion
# zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
# filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.(o|c~|old|pro|zwc)'
# add colors to completions
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# End of linesme suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.(o|c~|old|pro|zwc)'



# added by compinstall

# enable emacs mode
bindkey -e

setopt NO_BEEP

export PATH=$PATH:~/.bin

# }}}

# old school hotkeys {{{
bindkey '^[[5~' history-beginning-search-backward #pg-up
bindkey '^[[6~' history-beginning-search-forward #pg-dn
bindkey '^A' beginning-of-line #ctrl-a
bindkey '^[OH' beginning-of-line #pos-1
bindkey '^[[H' beginning-of-line #pos-1 on mac keyboard?
bindkey '[7~' beginning-of-line #urxvt
bindkey '^E' end-of-line #ctrl-e
bindkey '^[OF' end-of-line #end
bindkey '^[[F' end-of-line #end on mac keyboard?
bindkey '[8~' end-of-line #urxvt
bindkey '^[[3~' delete-char #del

# }}}

# aliases {{{

# general
(( ${+LS_COLORS} )) && alias ls='ls --color=auto'
alias l='ls --color=auto'
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -la --color=auto'
alias lsd='ls --color=auto -d */'
alias lad='ls -la --color=auto -d */'
alias lld='ls -l --color=auto -d */'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias cls='clear'
alias stardict='stardict -h'
alias conky-cli='conky'

# global
alias -g G='|grep -i'
alias -g L='|less'
alias -g pmc='pacman-color'
alias -g pm='pacman'

source ~/.zsh-aliases
# }}}

# env variables {{{

#setting editor to anything but vi obviously sets edit mode to emacs
#which is more straightforward
#should be covered by 'bindkey e' anyway
export EDITOR=${EDITOR:-/usr/bin/vim}
export PAGER=${PAGER:-`which less`}

#add kde 3.5 to path for pidgin to work with dcop while kde4 isn't ready
export PATH=$PATH:/usr/kde/3.5/bin:~/.bin

# }}}

# term-specific environment config {{{

# open (resume) a screen session if in a vt or on the server
case "$TERM" in 
	screen*)
		;;
	*screen)
		;;
	*screen*)
		#print -nR $'\033k'$1$'\033\\'
		#print -nR $'\033]0;'$2$'\a'
		precmd () { 
			print -Pn "\e]83;title \"$1\"\a" 
			print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~]\a" 
		}
		preexec () { 
			print -Pn "\e]83;title \"$1\"\a" 
			print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~] ($1)\a" 
		}
		;;
	*rxvt*|*term*)
		#precmd () {print -Pn "\e]o;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\a"}
		precmd() { print -Pn "\e]0;%n@%m - %~\a" }
		preexec () { print -Pn "\e]0;%n@%m - $1\a" }
		;;
	*)
		;;
esac

# }}}

# special notebook settings {{{

if [[ $HOST == "giggedy" ]]; then

	export OOO_FORCE_DESKTOP='gnome'
	
	alias startx='startx -- -dpi 75'
fi

# }}}

# special workstation settings {{{
	
	if [[ $HOST == "corrupted" ]]; then
		alias urxvt='urxvt -ip -tint black -sh 75'
	fi

# }}}

# Prompt color defines {{{

BLACK="%{"$'\033[01;30m'"%}"
GREEN="%{"$'\033[01;32m'"%}"
RED="%{"$'\033[01;31m'"%}"
YELLOW="%{"$'\033[01;33m'"%}"
BLUE="%{"$'\033[01;34m'"%}"
BOLD="%{"$'\033[01;39m'"%}"
NORM="%{"$'\033[00m'"%}"

# }}}

# prompt config {{{

if [ "$USERNAME" = root ]; then
	export PS1="${RED}%m ${BLUE}~ %# ${NORM}" 
else
	export PS1="${GREEN}%n@%m ${BLUE}~ %# ${NORM}"
fi

# }}}

# functions {{{
chpwd() {
  [[ -t 1 ]] || return
  case $TERM in
    sun-cmd) print -Pn "\e]l%~\e\\"
      ;;
    *xterm*|rxvt|(dt|k|E)term)
		print -Pn "\e]2;%~\a"
      ;;
  esac
}
# }}}
