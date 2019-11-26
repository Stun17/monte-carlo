for x in *.txt ; do sed -f conv.sed $x > ${x/%txt/dat} ; done
for x in *.dat ; do sort -nr -k2 $x > ${x/%dat/data} ; done
rm -f *.dat
for x in 2 3 4 5 6 7 8 9 0 
do 
  sed -i "s/^/$x /" ${x}.data ; 
  cat ${x}.data >> common_full.data ; 
done
sed -i 's/ -0\.0/ 0.0/ ; s/ -/-/ ;' common_full.data
