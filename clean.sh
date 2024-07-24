#!/bin/sh
rm -rf ./ana/*
rm -rf ./fsc/*
rm -rf ./bgd/*
cd ./truth/
make clean
rm -f tru*:*:* tru*.dat
#
cd ../letkf/
make clean
rm -f *.dat
#
cd ../ctl/
make clean
rm -f ctl_*:*:* ctl*.dat
#
cd ../ini/
make clean
#
cd ../dig/
make clean
#
cd ../obs/
make clean
rm -f obs_*:*:* obs*.dat
cd ../
rm -f *~
