# Scott's .zshrc
export CLOJURESCRIPT_HOME=~/src/clojurescript
path=(. ~/src/play20 ~/src/zathura ~/src/scala-2.9.1.final/bin ~/opt/ide/emacs/src ~/src/notmuch $CLOJURESCRIPT_HOME/bin ~/src/cljs-watch ~/src/git-notify ~/src/youtube-dl ~/.gem/ruby/1.9.1/bin ~/src/msmtpq ~/src/emacs/bin ~/.cabal/bin ~/.gem/ruby/1.8/bin  /usr/local/share/perl/5.10.1/auto/share/dist/Cope $path /bin /usr/bin /usr/local/bin . /usr/X11R6/bin ~/bin ~/src/apt-cyg ~/src/ant/bin ~/src/dejour/bin ~/.cljr/bin )

INFOPATH=( ~/doc/info )
export INFOPATH
# menu
zstyle ':completion:*' menu select=0
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
    
autoload -U compinit
compinit

# bc otherwise I get errors on computers that don't have notify-send
function notify { if [[ -x "/usr/bin/notify-send" ]]; then; notify-send $@; fi }

## Hosts
alias ssrn='sudo shutdown -r now'
alias ssn='sudo shutdown now'

# short for say done
alias sd='notify -t 2000 -i alert "Done"'
alias alsamixer='alsamixer -g'
alias o='gnome-open'
alias ka='killall'
alias tree='tree -C'
alias df='df -h'

# Aliases
alias create-tags='ctags -e -R *'
alias cm="./configure && make"
function mayberun() {if test -f $1; then $1; fi }
alias bui="mayberun ./autogen.sh && mayberun ./configure && make; notify -t 3000 'build done'"
alias smi='sudo make install && notify -t 3000 -i info "sudo make intall" "Done"'

## Lein
alias len='notify -t 2000 -i leiningen "Leiningen"' 
alias lei='lein install; len "install done"'
alias lej='lein jar; len "jar done"'
alias leu='lein uberjar; len "uberjar done"'
alias lec='lein clean; len "clean done"'
alias led='lein deps; len "dep done"'
alias les='len "swank starting"; lein swank'
alias les2='len "swank starting"; lein swank-clj'

## Maven
# alias mvni='mvn clean install -Dmaven.test.skip=true'
# alias mvnt='mvn test -Dtest='

## Git
alias g='git'
function gcl { git clone $1 && notify -t 3000 -i git "Git clone completed" "$1" }
alias gl='git log --pretty=format:"%h %ad | %s%d [%an]" --graph --date=short'
alias gst='git status'
alias gba='git branch -all'
alias gp='git pull'
alias gl='git log'
alias go='git co'
alias cx='chmod +x'
alias rmr="mv -t ~/.local/share/Trash/files"
alias rmrf='rm -rf'
alias dus='du -cBM --max-depth=1 | sort -n'
alias dum='du -cBM --max-depth=1'
alias ll='ls -lh'
alias ls='ls --color=always -F -h'
alias c='clear'
alias ps='ps aux'
alias k9='kill -9'
alias psg='ps G'
alias s='screen'
alias sv='sudo vim'
alias em='emacs -nw -q'
alias sem='sudo emacs -nw -q'
alias f='find ./ 2> /dev/null'
alias fig-names='find | grep'
alias fig-contents='grep -ir'
alias grep='grep --color -i'
alias more='less'
alias recent='ls -rl *(D.om[1,10])'
alias irc2="irssi -c irc.freenode.net -n scottj"
alias mp="mpl -speed 1.6 "
alias myspace="dlmsm"

function w { wget -c $@ && notify -i info "Wget download completed" "$"}
function a { aria2c -c $@ && notify -i info "aria2c download completed" "$"}
alias mkdir='mkdir -p'
alias mk='mkdir'
function cdm {mkdir $1 ; cd $1}

## Apt-get
#alias sea='apt-cache search'
sea() { apt-cache search $1 | grep -C 500 $1;}
alias sea2='sea --names-only'
alias aptn='notify -t 2000 -i debian "Apt-get"' 
function ins {sudo apt-get install -y $* &&  aptn "Installed $@"} 
compdef "_deb_packages uninstalled" ins
function rem {sudo apt-get remove -y $* && aptn "Removed $@"} 
compdef "_deb_packages installed" rem
alias upd='sudo apt-get update'
alias upg='sudo apt-get upgrade'
alias updg='upd; upg; aptn "update/upgrade done"'
alias arem='sudo apt-get autoremove -y'
alias arep='sudo add-apt-repository'

## Windows apt-cyg
if [[ $(uname -o) == Cygwin ]] {
        alias open=cygstart
        alias sa='eval `ssh-agent`; ssh-add'
        alias s='screen -s zsh-is-loading'
        alias sea='apt-cyg find'
        alias ins='apt-cyg install'
        alias rem='apt-cyg remove'
        alias upd='apt-cyg update'
        alias lein='lein.bat'
}

## Mac ports
if [[ $(uname) == Darwin ]] {
        alias ls="ls -G -F -h"
        alias ins="sudo port install"
        alias upd="sudo port -d selfupdate"
        alias sea="port search"
        alias rem="sudo port uninstall"
}

# Global Aliases
alias -g '...'='../..'
alias -g '....'='../../..'
alias -g '.....'='../../../..'
alias -g L='|less -R'
alias -g M='|more'
alias -g G='|grep'
alias -g T='|tail'
alias -g H='|head'
alias -g W='|wc -l'
alias -g S='|sort'

# environment settings
export EDITOR=emacsclient
export PAGER=less
export RSYNC_RSH=/usr/bin/ssh
export COLORTERM=yes
# for bug w/ buttons in eclipse not clicking when compiz is running
export GDK_NATIVE_WINDOWS=true
export WORDCHARS='*?[]~=&;!#$%^(){}'
# cdpath=(~)
bindkey -e

# # See if we can use colors. 
autoload colors zsh/terminfo 
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done

### Prompt $(print '%{\e[1;31m%}')
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}


zle_highlight+=(special:fg=blue
    region:bg=blue,fg=white,bold
    default:bold # ,fg=yellow
    isearch:fg=magenta)

# no color
# export PS1="
# %n@%m:%~:%# "

export PS1="
> "

# with color
# export PS1="
# %{$fg_bold[magenta]%}%n%{$reset_color%}@%{$fg_bold[green]%}%m%{$reset_color%}:%{$fg_bold[yellow]%}%~%{$reset_color%}:%# "

# export PS1="
# %{$fg_bold[yellow]$bg_bold[red]%}%n@%m%{$reset_color%}:%~%# "

# export PS1="
# %B%~%(?..[%?])%(#.#.) %b"

# %# is % or #
# %m is hostname

# export PS1="
# %B%# %b"
# export PS2='%B%_%(#.#.)%b_'
# export RPS1='%B%~%b '


# Completion, color
zstyle ':completion:*' completer _complete
ZLS_COLORS=$LS_COLORS
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors no=00 fi=00 di=01\;34 pi=33 so=01\;35 bd=00\;35 cd=00\;34 or=00\;41 mi=00\;45 ex=01\;32

### Enable advanced completions
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

### General completion technique
#zstyle ':completion:*' completer _complete _correct _approximate _prefix
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete

# Completion caching
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

# Expand partial paths
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes'
# Don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~'

# Separate matches into groups
zstyle ':completion:*:matches' group 'yes'

# Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b"

# Messages/warnings format
zstyle ':completion:*:messages' format '%B%U---- %d%u%b' 
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b'
 
# Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'

zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# kill
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# menu for kill
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# kill menu extension!
zstyle ':completion:*:processes' command 'ps --forest -U $(whoami) | sed "/ps/d"'
#zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:processes' insert-ids menu yes select

# remove uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
   adm alias apache at bin cron cyrus daemon ftp games gdm guest \
   haldaemon halt mail man messagebus mysql named news nobody nut \
   lp operator portage postfix postgres postmaster qmaild qmaill \
   qmailp qmailq qmailr qmails shutdown smmsp squid sshd sync \
   uucp vpopmail xfs

# case insensitivity, partial matching, substitution
zstyle ':completion:*' matcher-list 'm:{A-Z}={a-z}' 'm:{a-z}={A-Z}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'

# Common hostnames
if [[ "$ZSH_VERSION_TYPE" == 'new' ]]; then
  : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}}
else
  # Older versions don't like the above cruft
  _etc_hosts=()
fi

zstyle ':completion:*' hosts $hosts
zstyle ':completion:*:my-accounts' users-hosts $my_accounts
zstyle ':completion:*:other-accounts' users-hosts $other_accounts

#Zsh options
setopt no_beep
setopt share_history \
    hist_verify \
    no_clobber \
    auto_cd \
    complete_aliases \
    auto_list \
    complete_in_word \
    auto_pushd \
    # extended_glob \ # so I don't have to escape ^ in HEAD^
zle 



HISTFILE=~/.zsh/history
HISTSIZE=50000
SAVEHIST=50000

autoload -U url-quote-magic
zle -N self-insert url-quote-magic

notifyerr(){if [ "$?" -gt 0 ]; then notify -t 2000 -i error -u critical "${PREEXEC_CMD:-Shell Command}" "Error"; return -1; fi}

# Screen title names
precmd () {
    # must be first
    notifyerr
    # LASTCMD=$history[$((HISTCMD-1))]

    # BEGIN notify long running cmds
    stop=$(date +'%s')
	start=${PREEXEC_TIME:-$stop}
	let elapsed=$stop-$start
	max=${PREEXEC_MAX:-10}
	
	for i in ${PREEXEC_EXCLUDE_LIST:-}; do
	    if [ "x$i" = "x$PREEXEC_CMD" ]; then
	        max=999999;
	        break;
	    fi
	done
	
    if [ "${PREEXEC_CMD:-Shell Command}" != "s" ]; then
	    if [ $elapsed -gt $max ]; then
		    notify -t 2000 -i warning "${PREEXEC_CMD:-Shell Command}" "finished ($elapsed secs)" 
	    fi
    fi
    # END notify long running cmds
    if [[ $HOST == "mamey" ]]; then
        vcs_info
    fi
	my_pwd=$(print -P "%~")
	if [[ "$TERM" == "screen" ]]; then
		echo -ne "\ekzsh $my_pwd\e\\"
    fi
    # why always show hostname on local box?
    # if [ -n "$SSH_TTY" ] || [ "$(who am i | cut -f2  -d\( | cut -f1 -d:)" != "" ]; then
    if [ -n "$SSH_TTY" ]; then
        MAYBE_HOSTNAME="%m"
    fi
#         code=$(print -P "%?")
#     export RPS1="%t %{$fg_bold[yellow]%}$(parse_git_branch)%{$reset_color%} %{$fg_bold[red]%}%(?..[%?])%(#.#.)%{$reset_color%}%{$fg_bold[grey]%}"
#     export RPS1="${vcs_info_msg_0_} $(parse_git_branch) %(?..[%?])%(#.#.)"
    # export RPS1="${vcs_info_msg_0_} %(?..[%?])%(#.#.)"
    export RPS1="%~ $MAYBE_HOSTNAME %(?..[%?])%(#.#.)"

#         if [[ $code != "0" ]]; then
#             echo ok
#             export RPS1="$(parse_git_branch) %?"
#         fi

}

preexec () {
	if [[ "$TERM" == "screen" ]]; then
        local CMD=${1}
        echo -ne "\ek$CMD\e\\"
      fi
    # for notifying of long running commands
	export PREEXEC_CMD=`echo $1 | awk '{ print $1; }'`
	export PREEXEC_TIME=$(date +'%s')

}


# let alt backspace delete to separators. Use C-w to delete entire word regardless of separators
# backward-delete-to-slash () {
#     local WORDCHARS=${WORDCHARS//[&=-\/;.-!#%]}
#     zle .backward-delete-word
# }
# zle -N backward-delete-to-slash
bindkey "\e$terminfo[kbs]" backward-delete-word

# Print out current calendar with highlighted day
calendar() {
   if [[ ! -f /usr/bin/cal ]] ; then
      echo "Please install cal before trying to use it!"
      return
   fi
   if [[ "$#" = "0" ]] ; then
      /usr/bin/cal | egrep -C 40 --color "\<$(date +%e| tr -d ' ')\>"
   else
      /usr/bin/cal $@ | egrep -C 40 --color "\<($(date +%B)|$(date +%e | tr -d ' '))\>"
   fi
}

alias cal=calendar

# Remove useless files
clean () {
    if [ "$1" = "-r" ]; then
	find . \( -name '#*' -o -name '*~' -o -name '.*~' -o -name 'core*' \
	              -o -name 'dead*' \) -ok rm '{}' ';'
    else
	rm -i \#* *~ .*~ core* dead*
    fi
}

# Extract most types of archive
extract() {
   if [[ -z "$1" ]]; then
      print -P "usage: \e[1;36mextract\e[1;0m < filename >"
      print -P "       Extract the file specified based on the extension"
   elif [[ -f $1 ]]; then
      case ${(L)1} in
	  *.tar.bz2)  tar -jxvf $1;;
	  *.tar.gz)   tar -zxvf $1;;
	  *.bz2)      bunzip2 $1   ;;
	  *.gz)       gunzip $1   ;;
	  *.jar)      unzip $1       ;;
	  *.rar)      unrar x $1   ;;
	  *.tar)      tar -xvf $1   ;;
	  *.tbz2)     tar -jxvf $1;;
	  *.tgz)      tar -zxvf $1;;
	  *.zip)      unzip $1      ;;
	  *.Z)        uncompress $1;;
         *)          echo "Unable to extract '$1' :: Unknown extension"
      esac
   else
      echo "File ('$1') does not exist!"
   fi
}

compctl -g '*.tar.bz2 *.tar.gz *.bz2 *.gz *.jar *.rar *.tar *.tbz2 *.tgz *.zip *.Z' + -g '*(-/)' extract

# Create a diff
mdiff() { diff -udrP "$1" "$2" > diff.$(date "+%Y-%m-%d")."$1" }

# Reset current directory to sensible permissions
saneperms() {
    find . -type d -print0 | xargs -0 chmod 755
    find . -type f -print0 | xargs -0 chmod 644
}

function cs()
{
    cd $1;
    ls --color=auto -F -h
}
#alias cd='cs'

# scp-pull username@host.com tmp
function scp-pull()
{
  ssh $1 "tar czf - $2" | tar xzvf - -C ./
}
# summarized google, ggogle, mggogle, agoogle and fm
function search()
{
    case "$1" in
	-g) ${BROWSER:-lynx} http://www.google.com/search\?q=$2
	    ;;
	-u) ${BROWSER:-lynx} http://groups.google.com/groups\?q=$2
	    ;;
	-m) ${BROWSER:-lynx} http://groups.google.com/groups\?selm=$2
	    ;;
	-a) ${BROWSER:-lynx} http://groups.google.com/groups\?as_uauthors=$2
	    ;;
	-c) ${BROWSER:-lynx} http://search.cpan.org/search\?query=$2\&mode=module
	    ;;
	-f) ${BROWSER:-lynx} http://freshmeat.net/search/\?q=$2\&section=projects
	    ;;
	-F) ${BROWSER:-lynx} http://www.filewatcher.com/\?q=$2
	    ;;
	-G) ${BROWSER:-lynx} http://www.rommel.stw.uni-erlangen.de/~fejf/cgi-bin/pfs-web.pl\?filter-search_file=$2
	    ;;
	-s) ${BROWSER:-lynx} http://sourceforge.net/search/\?type=soft\&q=$2
	    ;;
	-w) ${BROWSER:-lynx} http://de.wikipedia.org/wiki/$2
	    ;;
	-W) ${BROWSER:-lynx} http://en.wikipedia.org/wiki/$2
	    ;;
	-d) lynx -source "http://dict.leo.org?$2" | grep -i "TABLE.*/TABLE" | sed "s/^.*\(<TABLE.*TABLE>\).*$/<HTML><BODY>\1<\/BODY><\/HTML>/" | lynx -stdin -dump -width=$COLUMNS -nolist;
;;
*) 
	    echo "Usage: $0 {-g | -u | -m | -a | -f | -c | -F | -s | -w | -W | -d}"
	    echo "-g:  Searching for keyword in google.com"
	    echo "-u:  Searching for keyword in groups.google.com"
	    echo "-m:  Searching for message-id in groups.google.com"
	    echo "-a:  Searching for Authors in groups.google.com"
	    echo "-c:  Searching for Modules on cpan.org."
	    echo "-f:  Searching for projects on Freshmeat."
	    echo "-F:  Searching for packages on FileWatcher."
	    echo "-G:  Gentoo file search."
	    echo "-s:  Searching for software on Sourceforge."
	    echo "-w:  Searching for keyword at wikipedia (german)."
	    echo "-W:  Searching for keyword at wikipedia (english)."
	    echo "-d:  Query dict.leo.org ;)"
	    esac
}

# for i in `seq 0 400`; do wget -r -H --level=1 -k -p -erobots=off -np -N http://foo.com/$i/bar; done

# rm() { mv "$@" ~/.recycle/ }

bindkey -s "^x^f" $'ec '
 
function lsx() { [[ -z $BUFFER ]] && BUFFER='ls' ; zle accept-line } ; zle -N lsx ; bindkey '^M' lsx
function cdx() { [[ -z $BUFFER ]] && BUFFER='cd' ; zle accept-line } ; zle -N cdx ; bindkey '^[\\' cdx

# alt-ret runs last command
function runprev() { zle up-line-or-history ; zle accept-line } ; zle -N runprev ; bindkey '^[^M' runprev

#function myal-updir() { if [[ -z $BUFFER ]] ; then BUFFER='..' ; zle accept-line ; else zle backward-delete-char ; fi } ; zle -N myal-updir ; bindkey '^?' myal-updir
#function myal-home() { if [[ -z $BUFFER ]] ; then BUFFER='~' ; zle accept-line ; else zle backward-kill-word ; fi } ; zle -N myal-home ; bindkey '^J' myal-home

function 700s() {for i in pic*; do convert -quality '95%' -resize 700 $i 700_$i; done}
function thumbs() {for i in pic*; do convert -thumbnail x100 $i th_$i; done}
function 700-and-thumb-file() {convert -quality '95%' -resize 700 $1 700_$1; convert -thumbnail x100 $1 th_$1; }

LS_COLORS='no=00:fi=00:di=00;94:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=00;92:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:';
export LS_COLORS

umask 022

function conkeror-history() {
    cp /cygdrive/c/Users/scott/AppData/Roaming/conkeror.mozdev.org/conkeror/Profiles/*.default/places.sqlite /tmp/
    echo "select p.url from moz_places as p, moz_historyvisits as h where p.url not like '%googleads%' and p.id = h.place_id order by h.visit_date DESC limit 100;" | sqlite3 /tmp/places.sqlite |less
    rm /tmp/places.sqlite
}

# zsh-git stuff
# setopt promptsubst
# autoload -U promptinit
# promptinit
# prompt wunjo


bmk_file=~/.bashrc_bmk
if [ -f $bmk_file ]; then
  . $bmk_file
fi
alias bmk_reload='. $bmk_file'
alias bmk_list="sort $bmk_file | awk 'BEGIN { FS = "'"[ =]" }; { printf("%-25s%s\n", $1, $2) }'"'"
alias pg='ping google.com'
alias pg1='ping -c 1 google.com'
auth()
{
# ex: auth user@host ~/.ssh/id_rsa.pub
  ssh $1 "cat - >> ~/.ssh/authorized_keys" < $2
}

# S-f10 in emacs saves dir, F10 in zsh goes there.
do-cd-emacs() {
       LBUFFER="cd $(cat ~/.emacs.d/current-directory)"
       zle accept-line
}
zle -N do-cd-emacs
bindkey '\e[21~' do-cd-emacs #F10

# export CLASSPATH="$HOME/.jars/*"

# makes it so ins and other aliases get proper completion
unsetopt completealiases

EAT_CONFIG=development
# ^xs for incremental forward search
unsetopt flowcontrol

# async browsing, kinda
function b {(sleep 600; conkeror "$*" ) &}

bindkey "\C-w" kill-region

REPORTTIME=10

alias flash32='mv ~/.mozilla/plugins- ~/.mozilla/plugins-flash-sucks'
alias flash64='mv ~/.mozilla/plugins-flash-sucks ~/.mozilla/plugins'

# export JDK_HOME=/usr/lib/jvm/java-6-sun/
# export JAVA_HOME=/usr/lib/jvm/java-6-sun/

function forever { while true; do $*; done}

setopt promptsubst
autoload -Uz vcs_info

#precmd () { vcs_info }

zstyle ':vcs_info:*' formats '%r:%s:%b'
zstyle ':vcs_info:*' enable git cvs svn hg bzr darcs

alias redshift='redshift -l 38.87:-77.025'

alias consume='find -type f -name "*.mp3" | while read mp3; do mplayer -af scaletempo -speed 1.8 $mp3; done'

alias col='setxkbmap us -variant colemak; '
alias qwe='setxkbmap us'
alias qwf='setxkbmap us'

alias wgetdir='wget -r -nH -l1 -np'

function ranger-cd {
  before="$(pwd)"
  python2.6 ~/src/ranger/ranger.py --fail-unless-cd "$@" || return 0
  after="$(grep \^\' ~/.config/ranger/bookmarks | cut -b3-)"
  if [[ "$before" != "$after" ]]; then
    cd "$after"
  fi
}
# bindkey -s "\C-o" "ranger-cd^m"
bindkey -s "\C-o" "emacsclient -n .^m"


# . ~/.zsh/live-command-coloring.sh

. ~/.zsh/secret

alias slp="sudo /etc/acpi/sleep.sh sleep"

alias dashboard='watch --interval=3600 dashboard.clj -a'

# so s -x shows pids
compdef s=screen

# M-?to chdir to the parent directory (can't use M-u bc it's follow link in urxvt)
# bindkey -s '\e.' '^U..^M'

# If AUTO_PUSHD is set, Meta-p pops the dir stack
bindkey -s '\ep' '^Upopd >/dev/null; dirs -v^M'

# alt-s inserts sudo at beginning of line
insert_sudo () { BUFFER="sudo $BUFFER"; zle end-of-line }
zle -N insert-sudo insert_sudo
bindkey "^[s" insert-sudo

# insert_less () { zle beginning-of-line; zle -U "less "; zle accept-line }
insert_less () { BUFFER="less $BUFFER"; zle end-of-line; zle expand-or-complete }
zle -N insert-less insert_less
bindkey "^[l" insert-less

alias cpv="rsync -poghb --backup-dir=/tmp/rsync -e /dev/null --progress --"

# see what's bound: bindkey '^p'
# or bindkey will show you what's bound to one key or all

# make xdg-open uses what gnome-open does
export DE="gnome"

alias -g ND='*(/om[1])' # newest directory
alias -g NF='*(.om[1])' # newest file

alias load-rvm='source "/home/scott/.rvm/scripts/rvm"'
# [[ -s "/$HOME/.rvm/scripts/rvm" ]] && . "/$HOME/.rvm/scripts/rvm"

# Opens the github page for the current git repository in your browser
function gh() {
    giturl=$(git config --get remote.origin.url)
    if [[ "$giturl" == "" ]]; then
        echo "Not a git repository or no remote.origin.url set"
        exit 1
    fi
    
    giturl=${giturl/git\@github\.com\:/https://github.com/}
    giturl=${giturl/\.git//}
    echo $giturl
    gnome-open $giturl
}

alias git="hub"

insert_cat () { BUFFER="cat $BUFFER"; zle end-of-line; zle expand-or-complete }
zle -N insert-cat insert_cat
bindkey "^[;" insert-cat

alias naut="nautilus --no-desktop"

function hold_package {
    sudo su -c 'echo $1 hold | dpkg --set-selections'
}

alias packages-installed="dpkg --get-selections"
alias package-files="dpkg -L"

fpath=(~/src/zsh-completions $fpath)

# exec 2>>( while read X; do print "\e[91m${X}\e[0m" > /dev/tty; done & )

alias top="top -d 20 -u scott"
alias dual-head-normal="xrandr --output LVDS --auto --output DVI-0 --auto --right-of LVDS --rotate normal"
alias dual-head-rotate="xrandr --output LVDS --auto --output DVI-0 --auto --right-of LVDS --rotate left"
alias one-head="xrandr --output LVDS --auto --output DVI-0 --off"

autoload -Uz run-help-git
alias ext="aunpack"

export CONS_PLAYBACK_SPEED=1.0

alias wireless-restart="killall nm-applet; nm-applet"
alias grooveshark="google-chrome http://listen.grooveshark.com/"
alias doubleclick="/usr/bin/xte 'mouseup 2' 'mouseclick 1' 'mouseclick 1' &"
alias notmuch="emacsclient -e '(notmuch)'"
alias hangup="phone.sh terminate"
alias answer="phone.sh answer"
alias previous-song-pandora="pianoctl -"
alias next-song-pandora="pianoctl +"
alias computer="gnome-open computer:///"
alias trash="gnome-open trash:///"
alias desktop="gnome-open ~/Desktop"
alias increase-volume="amixer sset PCM 10+ unmute"
alias decrease-volume="amixer sset PCM 10- unmute"
alias mute="amixer sset PCM mute"

setopt HIST_IGNORE_SPACE
