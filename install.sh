#! /bin/bash

# sudo apt install network-manager-gnome opam dconf-cli

rm ~/.bashrc
rm ~/.profile

ln -s ~/conf/.bashrc ~/.bashrc
ln -s ~/conf/.profile ~/.profile
ln -s ~/conf/.emacs.d/init/.emacs ~/.emacs
mkdir ~/.saves
touch ~/.paths

rm -f ~/.config/rofi
rm -f ~/.config/conky
rm -f ~/.local/share/rofi/themes
rm -rf ~/.config/picom

mkdir -p ~/.config/rofi
mkdir -p ~/.config/i3
mkdir -p ~/.local/share/rofi/themes
mkdir -p ~/.config/picom

echo "include /home/hpompougnac/conf/i3/config" > ~/.config/i3/config
cp ~/conf/i3/i3bar.conky.conf.template ~/conf/i3/i3bar.conky.conf
ln ~/conf/i3/i3bar.conky.conf ~/.config/i3/i3bar.conky.conf
ln ~/conf/i3/conky-i3bar ~/.config/i3/conky-i3bar
ln ~/conf/rofi/themes/onedark.rasi ~/.local/share/rofi/themes/onedark.rasi
ln ~/conf/rofi/config.rasi ~/.config/rofi/config.rasi
ln ~/conf/picom/picom.conf ~/.config/picom/picom.conf
