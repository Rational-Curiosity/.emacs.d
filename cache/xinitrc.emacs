#!/bin/sh
set -e
# You may need to comment out the next line to disable access control.
#xhost +SI:localuser:$USER

xrdb ~/.Xresources

# If Emacs is started in server mode, `emacsclient` is a convenient way to
# edit files in place (used by e.g. `git commit`).
export VISUAL=emacsclient
export EDITOR="$VISUAL"

xkbcomp -I$HOME/.emacs.d/cache/xkb $HOME/.emacs.d/cache/xkb/keymap/kbd_swap_ralt_ctrl $DISPLAY &

run_once () {
    pgrep -x $1 || "$@"
}
run_rplc () {
    pgrep -x $1 && pkill -x $1 || true
    "$@"
}

run_once compton -b --config /dev/null --backend xrender
run_once nm-applet &
run_once volumeicon &
run_rplc dunst -history_length 100 -history_key "mod4+e" \
         -lto 10s -nto 15 -cto 20 -show_age_threshold 1m -idle_threshold 10m \
         -format "%a: %s %n\n%b" &

case "$(uname -n)" in
OOOOO)
    # Place emacs directory into memory
    # type -p vmtouch && vmtouch -t ~/.emacs.d/el/

    export EXWM_MINIBUFFER_WORKSPACE_OR_SCREEN="LVDS-1"
    export EXWM_MONITOR_ORDER="LVDS-1 HDMI-1"
    # export EXWM_MONITOR_RESOLUTION="LVDS-1 640x480"
    ;;
gigas-29)
    # Place emacs directory into memory
    # type -p vmtouch && vmtouch -t ~/.emacs.d/el/ ~/.emacs.d/elpa/

    export EXWM_MINIBUFFER_WORKSPACE_OR_SCREEN="eDP-1"
    # ALC
    # export EXWM_MONITOR_ORDER="eDP-1 HDMI-1 DP-1"
    # MAD
    export EXWM_MONITOR_ORDER="DP-1 HDMI-1 eDP-1"
    # CON
    # export EXWM_MONITOR_ORDER="HDMI-1 eDP-1 DP-1"
    # export EXWM_MONITOR_RESOLUTION="HDMI-1 1280x720"
    ;;
esac

# Finally launch emacs.
# exec dbus-launch --exit-with-session emacs --exwm
exec emacs --exwm
