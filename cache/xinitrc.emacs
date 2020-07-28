#!/bin/sh
# You may need to comment out the next line to disable access control.
#xhost +SI:localuser:$USER

xrdb ~/.Xresources

# If Emacs is started in server mode, `emacsclient` is a convenient way to
# edit files in place (used by e.g. `git commit`).
export VISUAL=emacsclient
export EDITOR="$VISUAL"
# Alcala
export EXWM_MONITOR_ORDER="eDP-1 HDMI-1 DP-1"
# Congosto
# export EXWM_MONITOR_ORDER="HDMI-1 eDP-1 DP-1"
# export EXWM_MONITOR_RESOLUTION="HDMI-1 1280x720"

# Finally launch emacs.
exec dbus-launch --exit-with-session emacs --exwm
