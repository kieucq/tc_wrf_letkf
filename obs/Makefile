#
# [NOTE]
#       This is a makefile for the program that create
#       an obs set of data.
#
# [HISTORY]
#     - 26 Mar 2010: created by Chanh Kieu
#    
# [AUTHOR]
#       Chanh Q. Kieu
#       Dept of Meteorology
#       Vietnam National University
#       email: kieucq@atmos.umd.edu     
#
#========================================================
#
.IGNORE:

.SUFFIXES:.f90 .f .o

RM_LIST= *.o *.exe *~ *mod error.status
INCLUDE=../Registry
PROGRAM= obs.exe stationid.exe wrf3dvar_to_letkf.exe sounding_to_littler.exe cimss_to_letkf.exe
#LDOPTION=

include ../configure.letkf

.f.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90 -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

OBJS1=  module_common_utils.o mt19937ar.o  obs.o compute_err_z.o

OBJS2= stationid.o compute_err_z.o

OBJS3= wrf3dvar_to_letkf.o compute_err_z.o

OBJS5= cimss_to_letkf.o 

OBJS4= sounding_to_littler.o

all: $(PROGRAM)

obs.exe: $(OBJS1)
	$(FC) -o $@ $(FCFLAGS) $(OBJS1) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

stationid.exe: $(OBJS2)
	$(FC) -o $@ $(FCFLAGS) $(OBJS2) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

wrf3dvar_to_letkf.exe: $(OBJS3)
	$(FC) -o $@ $(FCFLAGS) $(OBJS3) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

cimss_to_letkf.exe: $(OBJS5)
	$(FC) -o $@ $(FCFLAGS) $(OBJS5) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

sounding_to_littler.exe: $(OBJS4)
	$(FC) -o $@ $(FCFLAGS) $(OBJS4) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

obs.o: module_common_utils.o mt19937ar.o compute_err_z.o
module_common_utils.o:
mt19937ar.o:
stationid.o: compute_err_z.o
wrf3dvar_to_letkf.o: compute_err_z.o
cimss_to_letkf.o:
compute_err_z.o:
sounding_to_littler.o:

clean:
	$(RM) $(RM_LIST)
