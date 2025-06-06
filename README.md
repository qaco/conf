# Debian installation

## Misc

### Timezone

```dpkg-reconfigure tzdata```

### Hibernation

Without hibernation, do ```systemctl suspend```

For hibernation you need:
+ [Configure GRUB](https://www.hibit.dev/posts/88/how-to-enable-hibernation-in-linux)
+ [Recompile the kernel](https://cateee.net/lkddb/web-lkddb/HIBERNATION.html)

## Internet

### Changer DNS

Follow this link : https://www.ionos.com/digitalguide/server/configuration/how-to-set-dns-on-debian/

## Desktop environment

update-alternatives: usage of « /usr/bin/i3 » to provide « /usr/bin/x-window-manager

```sudo update-alternatives --config x-window-manager```

Sinon :
* ```xdg-settings get default-web-browser```
* ```xdg-settings set default-web-browser firefox-esr.desktop```

With 4k monitors, putting ```Xcursor.size: 24``` in the file
```/etc/X11/Xresources/x11-common``` may be needed to fix the cursor
size.

### Mate + i3

In ```dconf-editor``` :

Change ```org -> mate -> desktop -> session -> required-components``` from
```marco``` to ```i3```.

In ```org -> mate -> desktop -> session -> required-component-list```, delete:
* ```filemanager``` (otherwise the background window will cover everything)
* ```panel``` (if you want zero panel)

### Picom

Identify class_g of a window : ```xprop WM_CLASS```

## Console

### Color in tty

```TERM=xterm-256color emacsclient -nw .bashrc``` needed;
but the same command, in GUI, breaks colors.

Need a conditional alias using (I guess)
```tty | grep tty > /dev/null && alias emacsclient="TERM=xterm-256color emacsclient"```

### Choose an emulator

Tilix is a good compromise:
* It works well with emacs (just needs the melpa package xclip for copy/paste)
* It provides 256 colors.
* It really supports Unicode.
* But it depends on GTk3 and does not provide a text configuration file.

I prefer mate-terminal (even if it is part of the Mate desktop environment).

In comparison:
* kitty does not work well with emacs in console.
* others (even urxvt) does not support Unicode well.

### Theme

(urxvt .Xdefaults syntax)

```
URxvt.font: xft:DejaVu Sans Mono:size=11
URxvt.boldfont: xft:DejaVu Sans Mono:bold:size=11

!! Colorscheme

! special
*.foreground: #b2b2b2
*.background: #292b2e
*.cursorColor: #e3dedd
! black
*.color0: #292b2e
*.color8: #292b2e
! red
*.color1: #f2241f
*.color9: #f2241f
! green
*.color2: #67b11d
*.color10: #67b11d
! yellow
*.color3: #b1951d
*.color11: #b1951d
! blue
*.color4: #4f97d7
*.color12: #4f97d7
! magenta
*.color5: #a31db1
*.color13: #a31db1
! cyan
*.color6: #28def0
*.color14: #28def0
! white
*.color7: #b2b2b2
*.color15: #b2b2b2
```
 
## Drivers

### Printers

```
sudo usermod -a -G lpadmin myusername
```

### Network

Wifi:
* Install ```network-manager```
* Install ```firmware-realtex``` (drivers)
* ```nmcli device wifi connect myessid password mypasswd```

* Note that nm-applet (from the package network-manager-gnome) is mutually exclusive with /etc/network/interfaces

### Nvidia GPU

Add to /etc/default/grub in order to avoid tty resolution issues:
```
GRUB_GFXMODE=1920x1080
GRUB_GFXPAYLOAD_LINUX=keep
```

Add to ~/.profile in order to avoir screen tearing:
```nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"```

Steam:
```
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt install libgl1-mesa-dri:i386 libgl1:i386
sudo apt-get upgrade steam -f
sudo apt install nvidia-driver-libs:i386
```

### Disks

Installing caja is the best solution.

Alternative, USB mounting with ```usbmount```:
* package created from https://github.com/rbrito/usbmount
* ```for f in /media/usb*; do echo "$f"; ls "$f"; done```
* ```FILESYSTEMS="vfat ext2 ext3 ext4 hfsplus ntfs exfat"```
in ```/etc/usbmount/usbmount.conf```
