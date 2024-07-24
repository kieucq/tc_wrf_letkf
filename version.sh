#!/bin/sh
version="tc_wrf_letkf_v3.1"
mkdir ../${version}
mkdir ../${version}/ana
mkdir ../${version}/bgd
mkdir ../${version}/fsc
mkdir ../${version}/data
mkdir ../${version}/model
mkdir ../${version}/run
mkdir ../${version}/data/avn
mkdir ../${version}/data/obs
mkdir ../${version}/data/obs/RADS
mkdir ../${version}/data/obs/RADA
mkdir ../${version}/data/obs/SATE
cp -r dig ../${version}/
cp -r eval ../${version}/
cp -r obs ../${version}/
cp -r ini ../${version}/
cp -r letkf ../${version}/
cp -r 3dvar ../${version}/
cp -r truth ../${version}/
cp -r wrfpost ../${version}/
cp -r ctl ../${version}/
cp -r ext ../${version}/
cp -r utils ../${version}/
cp -r Registry ../${version}/
cp -r script ../${version}/
cp -r tcvital ../${version}/
cp -r vortex_init ../${version}/
cp clean.sh  ../${version}/
cp configure.letkf   ../${version}/
cp install.sh  ../${version}/
cp readme  ../${version}/
cp version.sh ../${version}/

