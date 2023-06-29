#! /bin/bash

# xmodmap -e "keycode  61 = Up"
# xmodmap -e "keycode 108 = Left"
# xmodmap -e "keycode 135 = Down"
# xmodmap -e "keycode 105 = Right"

if xmodmap -pke | grep "keycode  61" | grep -q "Up"; then
    xmodmap -e "keycode  61 = exclam section exclam section exclamdown dead_abovedot exclamdown"
    xmodmap -e "keycode 108 = ISO_Level3_Shift NoSymbol ISO_Level3_Shift"
    xmodmap -e "keycode 135 = Menu NoSymbol Menu"
    xmodmap -e "keycode 105 = Control_R NoSymbol Control_R"
    xmodmap -e 'add control = Control_R'
else
    xmodmap -e 'clear control'
    xmodmap -e 'add control = Control_L'
    xmodmap -e "keycode  61 = Up"
    xmodmap -e "keycode 108 = Left"
    xmodmap -e "keycode 105 = Right"
    xmodmap -e "keycode 135 = Down"
fi
