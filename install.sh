#!/bin/sh

ENTRIES=$(ls -1d _*)
ORIGINALS_DIR="${PWD}/originals"

for ENTRY in $ENTRIES; do
  DOTFILE="${HOME}/.${ENTRY:1}"

  # check if dotfile already exists
  if [ -s "$DOTFILE" ]; then
    if [ -L "$DOTFILE" ]; then
      # dotfile exists as a symlink, remove it
      rm "$DOTFILE"
    else
      # dotfile exists as a real file, move it to defaults directory
      ORIGINAL_FILE="${ORIGINALS_DIR}/_${ENTRY:1}"
      [[ ! -d "$ORIGINALS_DIR" ]] && mkdir -p "$ORIGINALS_DIR"
      cmp --silent -- "$DOTFILE" "$ORIGINAL_FILE" || mv "$DOTFILE" "$ORIGINAL_FILE"
    fi
  fi

  ln -s "${PWD}/${ENTRY}" "$DOTFILE"
done
