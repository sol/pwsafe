#!/bin/bash
set -o errexit

pwsafe --lock

trap "pwsafe --unlock" SIGINT

filename=~/.pwsafe/db
tmp=`mktemp`
echo "using temporary file:  $tmp"

gpg -d $filename > $tmp
vim -n -i NONE -c "set nobackup" $tmp
mv $filename $filename.old
gpg -e --default-recipient-self --output $filename $tmp
shred -u $tmp

pwsafe --unlock
