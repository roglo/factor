#!/bin/sh
# $Id: pseudofact.sh,v 1.3 2009/12/18 09:39:42 deraugla Exp $

i=1; a=1; b=1; c=1; d=1; e=1; f=1
h=1
while true; do
   g=$(./pseudofact -abs $i)
j=$h
   h=$(./gcd $a $b $c $d $e $f $g)
   if [ "$i" -gt 6 ]; then
#     echo -n $((i-6)):
#     ./factor $h | sed -e 's/^.*://'
k=$(./pseudofact -fact $((i-6)))
echo -n $((i-6)):; ./factor $h/$(./gcd $h $k) | sed -e 's/^.*://'
# echo
   fi
   a=$b; b=$c; c=$d; d=$e; e=$f; f=$g
   i=$((i+1))
done
