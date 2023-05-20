# Debian installation

## Console

### Choose an emulator

Tilix is a good compromise:
* It provides 256 colors.
* It really supports Unicode.
* But it depends on GTk3 and does not provide a text configuration file.

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

### Network

Wifi:
* Install ```network-manager```
* Install ```firmware-realtex``` (drivers)
* ```nmcli device wifi connect myessid password mypasswd```

### Nvidia GPU

Add to /etc/default/grub in order to avoid tty resolution issues:
```
GRUB_GFXMODE=1920x1080
GRUB_GFXPAYLOAD_LINUX=keep
```

Add to ~/.profile in order to avoir screen tearing:
```nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"```

### Disks

Usb mounting with ```usbmount```:
* package created from https://github.com/rbrito/usbmount
* ```for f in /media/usb*; do echo "$f"; ls "$f"; done```
* ```FILESYSTEMS="vfat ext2 ext3 ext4 hfsplus ntfs exfat"```
in ```/etc/usbmount/usbmount.conf```
