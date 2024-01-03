#! /bin/bash

rm ~/.bashrc
rm ~/.profile

ln -s .bashrc ~/.bashrc
ln -s .profile ~/.profile
ln -s .emacs.d/init/.emacs ~/.emacs
ln -s .emacs.d/ ~/.emacs.d
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
ln i3/i3bar.conky.conf ~/.config/i3/i3bar.conky.conf
ln i3/config ~/.config/i3/config
ln i3/conky-i3bar ~/.config/i3/conky-i3bar
ln newsboat/config .config/newsboat/config
ln newsboat/urls .config/newsboat/urls
ln -s fontconfig/fonts.conf ~/.config/fontconfig/fonts.conf
ln rofi/themes/onedark.rasi ~/.local/share/rofi/themes/onedark.rasi
ln rofi/config.rasi ~/.config/rofi/config.rasi
ln picom/picom.conf ~/.config/picom/picom.conf
