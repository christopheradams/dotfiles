#!/bin/bash

# Check if directory is provided as argument
if [ "$#" -eq 1 ]; then
    cd "$1" || exit 1
    echo "Processing files in directory: $1"
else
    echo "Processing files in current directory"
fi

# Count files that will be processed
count=$(find . -maxdepth 1 -name "* \[*\].webm" | wc -l)
echo "Found $count files to rename"

# Process each file with the pattern "Name [ID].webm"
for file in * \[*\].webm; do
    # Skip if no matching files
    [ -e "$file" ] || continue

    # Extract the part before the YouTube ID
    new_name=$(echo "$file" | sed 's/ \[[^]]*\]\.webm$/\.webm/')

    # Verify the file hasn't already been renamed
    if [ "$file" != "$new_name" ]; then
        echo "Renaming: '$file' to '$new_name'"
        mv "$file" "$new_name"
    else
        echo "Skipping: '$file' (no change needed)"
    fi
done

echo "Rename complete!"
