#! /bin/bash

# sudo apt install network-manager-gnome opam dconf-cli

# terminal management
rm ~/.bashrc
rm ~/.profile
rm ~/.zshrc
ln -s ~/conf/.bashrc ~/.bashrc
cp ~/conf/.zshrc ~/.zshrc
ln -s ~/conf/.profile ~/.profile
cp ~/conf/.bash_aliases ~/.bash_aliases
touch ~/.paths

# terminal emulator management
rm -f ~/.config/kitty
mkdir ~/.config/kitty
ln -s kitty/kitty.conf ~/.config/kitty/kitty.conf
ln -s kitty/current-theme.conf ~/.config/kitty/current-theme.conf

# emacs management
ln -s ~/conf/.emacs.d/init.el ~/.emacs
mkdir ~/.saves

# desktop environment management
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
