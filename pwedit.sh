#!/bin/bash
set -o errexit

filename=~/.pwsafe/db
tmp=`mktemp`
echo "using temporary file:  $tmp"

gpg -d $filename > $tmp
vim -n -i NONE -c "set nobackup" $tmp
mv $filename $filename.old
gpg -e --default-recipient-self --output $filename $tmp
shred -u $tmp
