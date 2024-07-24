#!/bin/sh
cd ./truth/
make clean
make 
ln -sf ../namelist.letkf ./
ln -sf ../script/runtru.sh ./
#
cd ../letkf/
make clean
make 
ln -sf ../namelist.letkf ./
ln -sf ../script/runletkf.sh ./
#
cd ../ctl/
make clean
make 
ln -sf ../namelist.letkf ./
ln -sf ../script/runctl.sh ./
#
cd ../ini/
make clean
make
ln -sf ../namelist.letkf ./
ln -sf ../script/runini.sh ./
#
cd ../dig/
make clean
make
ln -sf ../namelist.letkf ./
ln -sf ../script/runrms.sh ./
cd ../obs/
#
make clean
make
ln -sf ../namelist.letkf ./
ln -sf ../script/runobs.sh ./
cd ../3dvar/
ln -sf ../script/run3dvar.sh ./
#
cd ../vortex_init/
make clean
make
ln -sf ../namelist.letkf ./
ln -sf ../script/runrms.sh ./
cd ../obs/
#
cd ../wrfpost/
make clean
make
ln -sf ../script/runwpost.sh ./


