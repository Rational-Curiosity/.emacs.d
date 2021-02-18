# setxkbmap -print > ~/.emacs.d/cache/xkb/keymap/mykbd
xkbcomp -I$HOME/.emacs.d/cache/xkb $HOME/.emacs.d/cache/xkb/keymap/kbd_swap_ralt_ctrl $DISPLAY
