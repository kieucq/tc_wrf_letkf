#!/bin/bash
#
# convert from gdas to little edian format
# ./ssrc.exe < ../data/gdas1.t00z.prepbufr.nr > test2_data.bufr
#
file=$1
pgf90 -o ${file}.exe ${file}.f90 -L/state/partition1/home/kieuc/opt/bufr -lbufr
