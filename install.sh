#! /bin/bash

rm ~/.bashrc
rm ~/.profile

ln -s ~/conf/.bashrc ~/.bashrc
ln -s ~/conf/.profile ~/.profile
ln -s ~/conf/.emacs.d/init/.emacs ~/.emacs
ln -s ~/conf/.emacs.d/ ~/.emacs.d
mkdir ~/.saves

rm -f ~/.config/rofi
rm -f ~/.config/conky
rm -f ~/.config/i3
rm -f ~/.config/newsboat
rm -f ~/.config/fontconfig
rm -f ~/.local/share/rofi/themes
rm -f ~/.config/picom

mkdir -p ~/.config/rofi
mkdir -p ~/.config/conky
mkdir -p ~/.config/i3
mkdir -p ~/.config/newsboat
mkdir -p ~/.config/fontconfig
mkdir -p ~/.local/share/rofi/themes
mkdir -p ~/.config/picom

echo "include /home/hpompougnac/conf/i3/config" > ~/.config/i3/config
ln ~/conf/i3/i3bar.conky.conf ~/.config/i3/i3bar.conky.conf
ln ~/conf/i3/conky-i3bar ~/.config/i3/conky-i3bar
ln ~/conf/rofi/themes/onedark.rasi ~/.local/share/rofi/themes/onedark.rasi
ln ~/conf/rofi/config.rasi ~/.config/rofi/config.rasi
ln ~/conf/picom/picom.conf ~/.config/picom/picom.conf
