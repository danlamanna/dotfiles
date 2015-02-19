export PATH=$PATH:/home/dan/bin

## GNOME PolicyKit and Keyring
eval $(gnome-keyring-daemon -s --components=pkcs11,secrets,ssh,gpg) &

## Set root window colour
hsetroot -solid "#2E3436" &

# setup dual monitors
/home/dan/.screenlayout/normal.sh &

# pnmixer &

if [ `which nm-applet` -eq 0 ]; then
   sudo nm-applet &
fi

## Set keyboard settings - 250 ms delay and 25 cps (characters per second) repeat rate.
## Adjust the values according to your preferances.
xset r rate 250 25 &

## Turn on/off system beep
xset b off &

# Custom
xmodmap /home/dan/.Xmodmap &

setlayout 0 3 2 0 &

claws-mail &

pidgin &

artha &

gtk-redshift -l42.6181:73.8367 -t 4400:5800 &

tint2 &

exec openbox-session
