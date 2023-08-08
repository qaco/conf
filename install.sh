#! /bin/bash

rm ~/.bashrc
rm ~/.profile

ln -s .bashrc ~/.bashrc
ln -s .profile ~/.profile
ln -s .emacs.d/init/.emacs ~/.emacs
ln -s conf/.emacs.d/ .emacs.d

rm -f ~/.config/conky
rm -f ~/.config/i3
rm -f ~/.config/newsboat
rm -f ~/.config/fontconfig
rm -f ~/.local/share/rofi/themes
rm -f ~/.config/picom
rm -f ~/.config/tilix

mkdir -p ~/.config/conky
mkdir -p ~/.config/i3
mkdir -p ~/.config/newsboat
mkdir -p ~/.config/fontconfig
mkdir -p ~/.local/share/rofi/themes
mkdir -p ~/.config/picom
mkdir -p ~/.config/tilix

ln conky/i3bar.conky.conf ~/.config/i3/i3bar.conky.conf
ln conf/i3/config ~/.config/i3/config
ln conf/i3/conky-i3bar ~/.config/i3/conky-i3bar
ln conf/newsboat/config .config/newsboat/config
ln conf/newsboat/urls .config/newsboat/urls
ln -s conf/fontconfig/fonts.conf ~/.config/fontconfig/fonts.conf
ln rofi/themes/onedark.rasi ~/.local/share/rofi/themes/onedark.rasi
ln conf/picom/picom.conf .config/picom/picom.conf
ln -s conf/tilix/one-dark.json .config/tilix/one-dark.json
