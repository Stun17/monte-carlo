for n in 2 3 4 5 6 7 8 9 0  
do 
  echo $n
  y=$( sed 'N;' "$n.r" ) ;
  for x in $y 
  do 
    grep $x $n.data >> $$
  done
  sort -nr -k2 $$
  rm $$
done
