#!/bin/bash

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <directory> <year>"
    exit 1
fi

input_dir="$1"
year="$2"

# Check if directory exists
if [ ! -d "$input_dir" ]; then
    echo "Error: Directory '$input_dir' does not exist."
    exit 1
fi

# Check if webm-to-mp3-song.sh exists and is executable
if ! command -v webm-to-mp3-song.sh &> /dev/null; then
    echo "Error: webm-to-mp3-song.sh script not found in PATH."
    echo "Make sure it's in your PATH or provide the full path to it."
    exit 1
fi

# Navigate to the input directory
cd "$input_dir" || exit 1

# Get album name (current directory name)
album=$(basename "$(pwd)")

# Get artist name (parent directory name)
artist=$(basename "$(dirname "$(pwd)")")

# Count total number of webm files
total_tracks=$(ls -1 *.webm 2>/dev/null | wc -l)

# Process each webm file
for webm_file in *.webm; do
    # Skip if no webm files exist
    [ -e "$webm_file" ] || continue

    # Extract filename without extension
    filename="${webm_file%.webm}"

    # Extract track number (assuming format like "01 Song Title")
    track_number=$(echo "$filename" | grep -o "^[0-9]\+")

    # Extract title (remove leading track number and space)
    title=$(echo "$filename" | sed "s/^$track_number //")

    # Clean up track number (remove leading zeros)
    track_number=$(echo "$track_number" | sed 's/^0*//')

    # Output filename
    output_file="${filename}.m4a"

    echo "Converting: $webm_file"
    echo "  Title: $title"
    echo "  Track: $track_number of $total_tracks"
    echo "  Album: $album"
    echo "  Artist: $artist"
    echo "  Year: $year"

    # Call the conversion script
    webm-to-mp3-song.sh "$webm_file" "$title" "$track_number" "$total_tracks" "$album" "$artist" "$year" "$output_file"

    # Check if conversion was successful
    if [ $? -eq 0 ]; then
        echo "Successfully converted to $output_file"
    else
        echo "Error converting $webm_file"
    fi

    echo "-----------------------------------"
done

echo "Conversion complete!"
