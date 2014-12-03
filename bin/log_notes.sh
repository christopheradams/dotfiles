#!/bin/sh
# Create a file for daily log notes

## date ##
NOW=$(date +"%Y-%m-%d %A, %B %-d")

LOGFILE="LOG $NOW.txt"

touch "/home/christopher/Notes/$LOGFILE"
