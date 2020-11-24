#!/bin/bash
export PATH=$PATH:~/.doom.d/hoth/
file = "$2"
# Remove spaces
mv "${file}" `echo $f | tr ' ' '_'`
hoth $1 "${`echo file | tr ' ' '_'`"
