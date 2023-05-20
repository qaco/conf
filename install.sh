#! /bin/bash

ln -s .bashrc ~/.bashrc
ln -s .emacs.d/init/.emacs ~/.emacs
ln -s conf/.emacs.d/ .emacs.d

mkdir -p ~/.config/conky
mkdir -p ~/.config/i3
mkdir -p ~/.config/newsboat
mkdir -p ~/.config/fontconfig
mkdir -p ~/.config/i3status

rm -f ~/.config/conky/*
rm -f ~/.config/i3/*
rm -f ~/.config/newsboat/*
rm -f ~/.config/fontconfig/*
rm -f ~/.config/i3status/*

ln conky/i3bar.conky.conf ~/.config/conky/i3bar.conky.conf
ln conf/i3/config ~/.config/i3/config
ln conf/i3/conky-i3bar ~/.config/i3/conky-i3bar
ln conf/newsboat/config .config/newsboat/config
ln conf/newsboat/urls .config/newsboat/urls
ln -s conf/fontconfig/fonts.conf ~/.config/fontconfig/fonts.conf
ln -s conf/i3status/config .config/i3status/config
