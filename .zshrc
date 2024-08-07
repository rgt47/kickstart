PS1='%m %B%2~ %(?:%F{green}☕%f:%F{red}☔%f)%b '
bindkey -v
autoload -U compinit && compinit -u && compinit && compdef _dirs d
setopt auto_cd auto_pushd pushd_ignore_dups pushdminus
cdpath=($HOME/Dropbox $HOME/Dropbox/prj $HOME/Dropbox/sbx $HOME/Dropbox/work )

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
source /usr/share/autojump/autojump.sh
alias eza=exa
source  /usr/share/zsh-autosuggestions/zsh-autosuggestions.zsh

elif [[ "$OSTYPE" == "darwin"* ]]; then
source $(brew --prefix)/share/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
source /opt/homebrew/etc/profile.d/autojump.sh
source ~/.iterm2_shell_integration.zsh
source  /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-you-should-use/you-should-use.plugin.zsh
fi
export TEXINPUTS='.:$HOME/shr/images:$HOME/shr:'
export BIBINPUTS='.:$HOME/shr/bibfiles:$HOME/shr'
export PATH=".:$HOME/bin:/opt/homebrew/sbin:/opt/homebrew/bin:$PATH"
HISTFILE="$HOME/.zsh_history"
HISTSIZE=10000000
SAVEHIST=10000000

setopt EXTENDED_HISTORY      # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY    # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY         # Share history between all sessions.
setopt HIST_IGNORE_DUPS      # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS  # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_SPACE     # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS     # Do not write a duplicate event to the history file.
setopt HIST_VERIFY           # Do not execute immediately upon history expansion.
setopt APPEND_HISTORY        # append to history file (Default)
setopt HIST_NO_STORE         # Don't store history commands
setopt HIST_REDUCE_BLANKS    # Remove superfluous blanks from each command line being added to the history.

alias hh=history
alias R='R --quiet --no-save'
alias mm='mutt'
alias sk='open -a Skim'
alias vc='vim ~/.vimrc'
alias vz='vim ~/.zshrc'
alias sz='source ~/.zshrc'
alias p2='enscript -C -2 -r -j --media=Letter'
alias p1='enscript  -j --media=Letter'
alias po='cd ~/prj/qblog/posts; lt'
alias yr="yabai --restart-service"
alias lt='eza -lrha -sold'
alias mvim="/Applications/MacVim.app/Contents/bin/mvim"
alias tp='trash-put -v'
alias rm='echo "This is not the command you are looking for."; false'
alias s='scd'
alias ZZ='exit'
alias rn="radian"
alias nt="nvim"
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='cd -'
alias 1='cd -1'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'

alias md='mkdir -p'
alias rd=rmdir

# List directory contents
alias lsa='ls -lah'
alias l='ls -lah'
alias ll='ls -lh'
alias la='ls -lAh'

# git
#
alias g='git'
compdef g=git
alias gst='git status'
compdef _git gst=git-status
alias gl='git pull'
compdef _git gl=git-pull
alias gup='git fetch && git rebase'
compdef _git gup=git-fetch
alias gp='git push'
compdef _git gp=git-push
gdv() { git diff -w "$@" | view - }
compdef _git gdv=git-diff
alias gc='git commit -v'
compdef _git gc=git-commit
alias gca='git commit -v -a'
compdef _git gca=git-commit
alias gco='git checkout'
compdef _git gco=git-checkout
alias gcm='git checkout master'
alias gb='git branch'
compdef _git gb=git-branch
alias gba='git branch -a'
compdef _git gba=git-branch
alias gcount='git shortlog -sn'
compdef gcount=git
alias gcp='git cherry-pick'
compdef _git gcp=git-cherry-pick
alias glg='git log --stat --max-count=5'
compdef _git glg=git-log
alias glgg='git log --graph --max-count=5'
compdef _git glgg=git-log
alias gss='git status -s'
compdef _git gss=git-status
alias ga='git add'
compdef _git ga=git-add
alias gm='git merge'
compdef _git gm=git-merge
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'
# source ~/.config/zsh/.zsh_functions
# source ~/.config/zsh/.zsh_exports
function d () {
  if [[ -n $1 ]]; then
    dirs "$@"
  else
    dirs -v | head -n 10
  fi
}

mma () { /Applications/Mathematica.app/Contents/MacOS/WolframKernel -script $1 }
function gz() {
    git add .
    git commit -a -m "$1"
    git push
}

export EDITOR="vim"
export TEXINPUTS='.:/Users/zenn/shr/images:/Users/zenn/shr:'
export PATH=".:.local/bin:/opt/homebrew/sbin:/opt/homebrew/bin:$PATH:$HOME/bin"
export vpc_id="vpc-14814b73"
export subnet_id="subnet-f02c90ab"
export ami_id="ami-014d05e6b24240371"
export instance_type="t2.micro"
export storage_size="30"
export keypair_name="power1_app"
export proj_name="power1_app"
export security_grp="sg-0b5a8c5d0c7d874ff"
export static_ip='54.177.217.40'
export HOMEBREW_AUTO_UPDATE_SECS="604800"

if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg -uuu --files --hidden --follow --no-ignore-vcs'
  export FZF_DEFAULT_OPTS='-m --height 50% --border --reverse --preview "cat {}"'
fi
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=red,bold,underline'
# export LSCOLORS='ExGxDxDxCxDxDxFxFxexEx'


ff() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}
export FZF_CTRL_R_OPTS="
  --bind 'ctrl-r:up'
  --bind 'ctrl-s:down'"

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

path=('/Users/zenn/.juliaup/bin' $path)
export PATH

# <<< juliaup initialize <<<
