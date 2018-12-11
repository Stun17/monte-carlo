#!/run/current-system/sw/bin/bash

./a.out 1000 2 |grep $1 |awk -e '{print $2,$3} END {print NR}'
