#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
set -eu

if [ "$#" -ne 2 ]; then
    echo "usage: $0 path/to/libsuz.so destination-directory" >&2
    exit 1
fi

library=$1
destination=$2

mkdir -p "$destination"
cp "$library" "$destination/"
ldd "$library" | awk '/libHS|libffi/ { print $3 }' | while read -r dependency; do
    cp "$dependency" "$destination/"
done
