#!/bin/bash
./${1}genhsvd.exe F1-$2.txt 1 $2 $1-$2-1-1 &
for ((I=1;I<=16;++I))
do
	./${1}genhsvd.exe F$I-$2.txt 2 $2 $1-$2-2-$I &
done
wait
rm $1-$2-1-1.J
mv $1-$2-1-1.LY $1-$2-1-1.EY
for ((I=1;I<=16;++I))
do
	rm $1-$2-2-$I.J
	mv $1-$2-2-$I.Y $1-$2-2-$I.W
	mv $1-$2-2-$I.LY $1-$2-2-$I.EW
done
