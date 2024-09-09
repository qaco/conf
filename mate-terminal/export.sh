#!/bin/bash
dconf dump /org/mate/terminal/global/ > mate-terminal-global.dconf
dconf dump /org/mate/terminal/profiles/ > mate-terminal-profiles.dconf
