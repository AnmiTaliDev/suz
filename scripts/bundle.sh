#!/bin/sh
# SPDX-License-Identifier: GPL-3.0-or-later
set -eu

if [ "$#" -ne 2 ]; then
    echo "usage: $0 path/to/libsuz.so.X.Y.Z destination-directory" >&2
    exit 1
fi

library=$1
destination=$2

mkdir -p "$destination"
cp "$library" "$destination/"
name=$(basename "$library")
soname=${name%.*.*}
ln -sf "$name" "$destination/$soname"
ln -sf "$soname" "$destination/${soname%.*}"
ldd "$library" | awk '/libHS|libffi/ { print $3 }' | while read -r dependency; do
    cp "$dependency" "$destination/"
done
