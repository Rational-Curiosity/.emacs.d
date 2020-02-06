#!/bin/sh
# You may need to comment out the next line to disable access control.
#xhost +SI:localuser:$USER

xrdb ~/.Xresources

# If Emacs is started in server mode, `emacsclient` is a convenient way to
# edit files in place (used by e.g. `git commit`).
export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Finally launch emacs.
exec dbus-launch --exit-with-session emacs --exwm
