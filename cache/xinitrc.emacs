#!/bin/sh
# You may need to comment out the next line to disable access control.
#xhost +SI:localuser:$USER

xrdb ~/.Xresources

# Place emacs directory into memory
vmtouch -vt ~/.emacs.d/

# If Emacs is started in server mode, `emacsclient` is a convenient way to
# edit files in place (used by e.g. `git commit`).
export VISUAL=emacsclient
export EDITOR="$VISUAL"
export EXWM_MINIBUFFER_WORKSPACE_OR_SCREEN="eDP-1"
# Alcala
# export EXWM_MONITOR_ORDER="eDP-1 HDMI-1 DP-1"
# Madrid
export EXWM_MONITOR_ORDER="DP-1 HDMI-1 eDP-1"
# Congosto
# export EXWM_MONITOR_ORDER="HDMI-1 eDP-1 DP-1"
# export EXWM_MONITOR_RESOLUTION="HDMI-1 1280x720"
# Finally launch emacs.
exec dbus-launch --exit-with-session emacs --exwm
