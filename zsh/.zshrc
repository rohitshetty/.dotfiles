# ~/.zshrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY
setopt APPEND_HISTORY

# enable color support of ls and add handy aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Custom aliases
alias tm='task-master'
alias taskmaster='task-master'
alias mpdstart='mpd ~/.config/mpd/mpd.conf'

# Alert alias for long running commands
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Locale
LANG="en_IN.utf8"
export LANG
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export TERM="screen-256color"

# Powerline
powerline-daemon -q
if [ -f /usr/share/powerline/bindings/zsh/powerline.zsh ]; then
    . /usr/share/powerline/bindings/zsh/powerline.zsh
elif [ -f "$HOME/.local/lib/python3.*/site-packages/powerline/bindings/zsh/powerline.zsh" ]; then
    . "$HOME"/.local/lib/python3.*/site-packages/powerline/bindings/zsh/powerline.zsh
fi

# hyfetch on shell start
hyfetch --ascii-file ~/.config/neofetch/b.txt -m 8bit

# z.lua - directory jumper
eval "$(lua ~/apps/z.lua/z.lua --init zsh enhanced once fzf)"

# PATH
export N_PREFIX=$HOME/.local
export PATH=$PATH:/home/rohit/apps
[ -f "$HOME/.cargo/env" ] && source "$HOME/.cargo/env"
[ -d "/usr/local/go/bin" ] && export PATH=$PATH:/usr/local/go/bin

# pnpm
export PNPM_HOME="/home/rohit/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

# bun
export BUN_INSTALL="$HOME/.bun"
[ -d "$BUN_INSTALL/bin" ] && export PATH="$BUN_INSTALL/bin:$PATH"

# Pomodoro timer
function pomo() {
    arg1=$1
    shift
    args="$*"

    min=${arg1:?Example: pomo 15 Take a break}
    sec=$((min * 60))
    msg="${args:?Example: pomo 15 Take a break}"

    while true; do
        date '+%H:%M' && sleep "${sec:?}" && notify-send -u critical -t 2500 -a pomo "${msg:?}"
    done
}

# Quick reminder using at
remind() {
  local amount="$1"
  local unit="$2"

  if [[ "$unit" =~ ^(minute|minutes|hour|hours|day|days)$ ]]; then
    shift 2
  else
    unit="minutes"
    shift 1
  fi

  echo "notify-send \"Reminder\" \"$*\"" \
    | at now + "$amount $unit"
}
