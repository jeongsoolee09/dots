# tmux on startup (ssh)
# This should be at the top!
if [[ $- == *i* ]] && [[ -z "$TMUX" ]] && [[ -n "$SSH_CONNECTION" ]]; then
    tmux attach-session -t ssh || tmux new-session -s ssh
fi

# tmux on startup (local)
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
  tmux new-session -A -s local
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# enable option+arrow keys!
bindkey "\e\e[D" backward-word
bindkey "\e\e[C" forward-word

# for z
. /opt/homebrew/etc/profile.d/z.sh

# fix zsh slow in git repo
__git_files () {
    _wanted files expl 'local files' _files
}

# Syntax highlighting in Terminal
export CLICOLOR=1

# emacs-mode in Terminal
set -o emacs

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Path to your oh-my-zsh installation.
export ZSH="/Users/jslee/.oh-my-zsh"

source ~/.powerlevel10k/powerlevel10k.zsh-theme

# case-sensitive completion
CASE_SENSITIVE="true"

# hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# enable command auto-correction
ENABLE_CORRECTION="true"

# display red dots whilst waiting for completion
COMPLETION_WAITING_DOTS="true"

plugins=(git
         bundler
         dotenv
         macos)

ZSH_DISABLE_COMPFIX="true"

source $ZSH/oh-my-zsh.sh

alias infer='~/Taint-Analysis/Code/infer/infer/bin/infer'
alias myssh='ssh jslee@163.152.26.56'
alias ariash='ssh jslee@163.152.26.211'
alias ytmp3='yt-dlp -ic -o "%(playlist_index)s-%(title)s.%(ext)s" --yes-playlist -x --audio-format mp3 --audio-quality 0 '
alias setjava8='export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_251.jdk/Contents/Home'
alias setjava11='export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk-11.0.8.jdk/Contents/Home'
alias infertop='~/Taint-Analysis/Code/infer/scripts/infer_repl'
alias commitdate='git commit -m "$(date "+%H:%M:%S   %d/%m/%y")"'
alias nuget="mono /usr/local/bin/nuget.exe"
alias mp3="mpv --no-video --quiet"
alias mp32="mpv --no-video --really-quiet"
alias fm4u="mpv --no-video --really-quiet https://korradio.stream/mbc_fm4u.pls"
alias brewupdate="brew update && brew upgrade && brew cleanup"
alias twitch='read streamer ; streamlink --player="mpv --no-video" https://twitch.tv/$streamer BEST'
alias tmuxlocal='tmux attach -t local'
alias tmuxssh='tmux attach -t ssh'
alias fv='vim $(fzf)'
alias fw='w3m $(fzf)'
alias wifi='networksetup -setairportpower en0'
alias wifistatus='networksetup -getairportpower en0'
alias battery='pmset -g batt'
alias ws='wifistatus; blueutil -p; battery'
alias ss='source ~/.zshrc'
alias playvideo='mpv --no-config --vo=tct '
alias pfind='ps -ef | grep -i '

alias brex='/usr/local/Homebrew/bin/brew'
alias brew='/opt/homebrew/bin/brew'

# ============ ls related ============

alias ls='exa -a'
alias sl='exa -a'
alias la="exa -la"

# ============ vim related ============

alias vi="/opt/homebrew/bin/vim"
alias vim='nvim'
alias vs='nvim -O'
alias sp='nvim -o'

# ============ emacs related ============

alias emacs='emacs -nw'
alias e='emacsclient -t'
alias ef='emacsclient $(fzf)'

# ============ rust alternatives ===========

# alias grep="rg"

# check dumb terminal for MacVim
if [[ "$TERM" == "dumb" ]]
then
   unsetopt zle
   # unsetopt prompt_cr
   unsetopt prompt_subst
   PS1='$ '
fi

source ~/.zshenv

alias dotfiles='/usr/bin/git --git-dir=/Users/jslee/.dotfiles.git/ --work-tree=/Users/jslee'
[ -f "/Users/jslee/.ghcup/env" ] && source "/Users/jslee/.ghcup/env" # ghcup-env

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# >>> scala-cli completions >>>
fpath=("/Users/jslee/Library/Application Support/ScalaCli/completions/zsh" $fpath)
compinit
# <<< scala-cli completions <<<
