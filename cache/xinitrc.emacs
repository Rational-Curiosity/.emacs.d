#!/bin/sh
# You may need to comment out the next line to disable access control.
#xhost +SI:localuser:$USER

xrdb ~/.Xresources

# If Emacs is started in server mode, `emacsclient` is a convenient way to
# edit files in place (used by e.g. `git commit`).
export VISUAL=emacsclient
export EDITOR="$VISUAL"

compton -b --config /dev/null --backend xrender
nm-applet &
volumeicon &

case "$(uname -n)" in
OOOOO)
    # Place emacs directory into memory
    type -p vmtouch && vmtouch -t ~/.emacs.d/el/

    export EXWM_MINIBUFFER_WORKSPACE_OR_SCREEN="LVDS-1"
    export EXWM_MONITOR_ORDER="LVDS-1 HDMI-1"
    # export EXWM_MONITOR_RESOLUTION="LVDS-1 640x480"
    ;;
gigas-29)
    # Place emacs directory into memory
    type -p vmtouch && vmtouch -t ~/.emacs.d/el/ ~/.emacs.d/elpa/

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
exec dbus-launch --exit-with-session emacs --exwm
