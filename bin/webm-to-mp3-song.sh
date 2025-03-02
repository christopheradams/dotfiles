#!/bin/bash

# Check if correct number of arguments is provided
if [ "$#" -lt 7 ]; then
    echo "Usage: $0 <input.webm> <title> <track_number> <total_tracks> <album> <artist> <year> <output.m4a>"
    echo "Example: $0 input.webm \"Song Name\" 1 12 \"Album Name\" \"Artist Name\" output.m4a"
    exit 1
fi

input_file="$1"
title="$2"
track_number="$3"
total_tracks="$4"
album="$5"
artist="$6"
year="$7"
output_file="$8"

# Check if input file exists
if [ ! -f "$input_file" ]; then
    echo "Error: Input file '$input_file' does not exist"
    exit 1
fi

# Extract temporary cover art
temp_cover="temp_cover_$$.jpg"
ffmpeg -i "$input_file" -vframes 1 "$temp_cover"

# Convert to m4a with metadata
ffmpeg -n -i "$input_file" -i "$temp_cover" \
    -map 0:a -map 1 \
    -c:a aac -q:a 2 \
    -c:v mjpeg -disposition:v attached_pic \
    -metadata title="$title" \
    -metadata track="$track_number/$total_tracks" \
    -metadata album="$album" \
    -metadata artist="$artist" \
    -metadata date="$year" \
    -metadata:s:v title="Album cover" \
    -metadata:s:v comment="Cover (front)" \
    "$output_file"

# Clean up
rm "$temp_cover"

echo "Conversion complete: $output_file"
