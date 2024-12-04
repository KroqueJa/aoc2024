#!/bin/sh

# Usage: ./day3.sh [input_file]

tr -d '\n' < $1|grep 'mul([0-9]\+,[0-9]\+)' -o|cut -c 5-|sed 's|.$||'|awk -F, '{sum+=$1*$2;} END{print sum}'

tr -d '\n' < $1|grep "mul([0-9]\+,[0-9]\+)\|do()\|don't()" -o|sed "s|'||g"|awk 'BEGIN {flag=1;} {if ($0=="do()") {flag=1} else if ($0=="dont()"){flag=0} else if (flag==1) {print $0}}'|cut -c 5-|sed 's|.$||'|awk -F, '{sum+=$1*$2;} END{print sum}'
