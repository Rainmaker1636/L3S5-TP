#!/bin/sh
GENOME=./aleazard
COMPTEUR=./compteur-gc
GENOME_INPUT=genome.txt
SIZE_N_FILE=n.dat
SIZE_T_FILE=t.dat
TIME_FILE=genom.dat
TEMP_FILE=tmp.dat
RES_FILE=res.dat
TIME_CMD="/usr/bin/time"
TIME_OPT="-f %e"

rm -f $SIZE_N_FILE $SIZE_T_FILE $TIME_FILE $RES_FILE
for n in `awk 'BEGIN{ for (i=100;i<1000000000;i*=10) print i }'`;
do
    echo $n 
    $GENOME $n > $GENOME_INPUT
    for t in `awk ' BEGIN { for (i=1;i<=32;i*=2) print i } '`;
    do
	echo $t
	echo $n >> $SIZE_N_FILE 2>&1
	echo $t >> $SIZE_T_FILE 2>&1
	$TIME_CMD "$TIME_OPT" $COMPTEUR $GENOME_INPUT $t > /dev/null 2>> $TIME_FILE
    done
done

paste $SIZE_N_FILE $TIME_FILE > $TEMP_FILE
paste $SIZE_T_FILE $TEMP_FILE > $RES_FILE
