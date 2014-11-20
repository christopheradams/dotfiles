#!/bin/sh

iwconfig wlan0 2>&1 | grep -q no\ wireless\ extensions\. && {
  echo wired
  exit 0
}

essid=`iwconfig wlan0 | awk -F '"' '/ESSID/ {print $2}'`
stngth=`iwconfig wlan0 | awk -F '=' '/Quality/ {print $2}' | cut -d '/' -f 1`
bars=`expr $stngth / 10`

case $bars in
  0)     bar='[-----]' ;;
  1|2)   bar='[/----]' ;;
  3|4)   bar='[//---]' ;;
  5|6)   bar='[///--]' ;;
  7|8)   bar='[////-]' ;;
  9|10)  bar='[/////]' ;;
  *)     bar='[--!--]' ;;
esac

echo $essid $bar

exit 0
