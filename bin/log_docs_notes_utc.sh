#!/bin/sh
# Create a file for daily log notes. Should be run at 12pm UTC.

## get tomorrow's date ##
DAY=$(date -d "1 day" +"%Y-%m-%d %A, %B %-d")

LOGFILE="LOG $DAY.txt"

touch "/home/christopher/Documents/Notes/$LOGFILE"
