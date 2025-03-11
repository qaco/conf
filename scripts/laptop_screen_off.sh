#!/bin/sh
xrandr --output eDP-1 --off
xrandr --output HDMI-1 --off
xrandr --output DP-1 --off
xrandr --output DP-2 --off
xrandr --output HDMI-1 --auto
xrandr --output DP-1 --auto
xrandr --output DP-2 --auto
feh --bg-fill ~/Images/nasa.png
