cp -f ../model/wrfinput_d01_orig ./bgd_010_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_001_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_002_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_003_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_004_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_005_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_006_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_007_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_008_00:00:00.dat
cp -f ../model/wrfinput_d01_orig ./bgd_009_00:00:00.dat
rm -f error_status
pgf90 -c mt19937ar.f90
pgf90 -c module_common_utils.f90
pgf90 -o ini.exe ini.f90 mt19937ar.f90 module_common_utils.f90 -I/usr/netcdf4.0/include /usr/netcdf4.0//lib/libnetcdf.a
