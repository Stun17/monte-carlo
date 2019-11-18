while read x
do
  grep $x *.data
done < $1 
