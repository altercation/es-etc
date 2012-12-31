# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
#export ZSH_THEME="robbyrussell"
#export ZSH_THEME="dstufft" #with slow hg support
#export ZSH_THEME="dst" # overall one of the best, though error code is just "fail"
#export ZSH_THEME="jreese" #good basic setup but only git
#export ZSH_THEME="mikeh" #interesting, nice compartmentalized data, can do hg
#export ZSH_THEME="pmcgee" #not bad, nice two line display
#export ZSH_THEME="sporty_256" #very nice color scheme, tight layout but no full path
#export ZSH_THEME="takashiyoshida" #similar concept as sporty, clean colors
#export ZSH_THEME="wezm" #very nice tight layout, minimal func
#export ZSH_THEME="wezm+" #pretty good for a single line layout, min, has error codes
#export ZSH_THEME="gozilla"
#export ZSH_THEME="../../conf/shell/brute"
export ZSH_THEME="../../$CONF_DIR/shell/brute"

# export DISABLE_AUTO_UPDATE="true"

#plugins=(git vi-mode)
#plugins=(git)
plugins=(vi-mode)

source $ZSH/oh-my-zsh.sh
bindkey -v

# the following addresses problems with the oh-my-zsh vim plugin eating the
# line above the prompt with resetting it. I've had no negative impact from
# nullifying the function of zle-line-init
function zle-line-init {
}

# start x session if not already in one
# modified from http://pbrisbin.com/pages/display-manager.html
if [[ $USER != "root" ]] && [[ `tty` = /dev/tty1 ]] && [[ -z "$DISPLAY" ]]; then
  startx
  logout
fi

