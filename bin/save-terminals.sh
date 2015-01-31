#!/usr/bin/env bash

CONFIG=/home/christopher/terminals.config
gnome-terminal --save-config=$CONFIG

LINES=($(grep -n '\[Terminal' $CONFIG | cut -d: -f1))
for ((i=0; i<$(grep '\[Terminal' $CONFIG | wc -l); i++))
do
    TITLE=$(xprop -id $WINDOWID WM_NAME | sed -e 's/WM_NAME(STRING) = "//' -e 's/"$//';xdotool key ctrl+Page_Down;)
    sed -ri "$((${LINES[$i]}+$i))s/.*/&\nTitle=$TITLE/" $CONFIG 
done
