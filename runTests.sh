#!/bin/bash

ghc blackMain.hs
echo "blackMain compiled."

ghc whiteMain.hs
echo "whiteMain compiled."

lim=250
printInterval=5

echo "Running AI."
./blackMain | sed 's/.*: //' |& tail -1 > blackResult.txt
./whiteMain | sed 's/.*: //' |& tail -1 > whiteResult.txt

i=2

while [ $i -le $lim ]
do
    ./blackMain | sed 's/.*: //' |& tail -1 >> blackResult.txt
    ./whiteMain | sed 's/.*: //' |& tail -1 >> whiteResult.txt
    if [ "$(($i % $printInterval))" -eq 0 ]
    then
        echo "$i done"
    fi
    i=$(($i+1))
done
