#!/run/current-system/sw/bin/bash

case $# in 
  3) ./a.out $1 $2 |grep $3 |awk -e '{print $2,$3} END {print NR}' ;;
  *) echo "usage: #hands #players high|pair|dupal|set|str8|flush|full|caree|fl-st" ;;
esac
