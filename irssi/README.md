Keep the config free of passwords

In `.gitattributes`:

    config filter=pw

In `$GIT_DIR/config`:

    [filter "pw"]
        clean = "sed -e 's/msg nickserv identify .* ;wait 2000\";/msg nickserv identify <PASSWORD> ;wait 2000\";/'"
