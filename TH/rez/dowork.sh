#for x in *.txt; do sed -f conv.sed $x > ${x/%txt/dat} ; done
for x in *.dat; do sort -nr -k2 $x > ${x/%dat/d} ; done
