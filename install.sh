#!/bin/sh

ENTRIES=$(ls -1d _*)
DEFAULTS_DIR="${PWD}/defaults"

for ENTRY in $ENTRIES; do
    DOTFILE="${HOME}/.${ENTRY:1}"

    # check if dotfile already exists
    if [ -s "$DOTFILE" ]; then
        if [ -L "$DOTFILE" ]; then
            # dotfile exists as a symlink, remove it
            rm "$DOTFILE"
        else
            # dotfile exists as a real file, move it to defaults directory
            [[ ! -d "$DEFAULTS_DIR" ]] && mkdir -p "$DEFAULTS_DIR"
            mv "$DOTFILE" "${DEFAULTS_DIR}/_${ENTRY:1}"
        fi
    fi

    ln -s "${PWD}/${ENTRY}" "$DOTFILE"
done
