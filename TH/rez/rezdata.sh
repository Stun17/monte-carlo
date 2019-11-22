while read x
do
  grep $x $2.data
done < $1 
