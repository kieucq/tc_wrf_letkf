#
# [NOTE]
#       This is a makefile for the program that create
#       an ensemble of initial cold start for letkf.
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

RM_LIST= *.o ini.exe *~ *mod error_status
INCLUDE=../Registry
PROGRAM= ini.exe
#LDOPTION=

include ../configure.letkf

.f.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90 -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

OBJS=  ini.o  module_common_utils.o  mt19937ar.o compute_err_z.o

all: $(PROGRAM)

ini.exe: $(OBJS)
	$(FC) -o $@ $(FCFLAGS) $(OBJS) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a


ini.o: module_common_utils.o mt19937ar.o compute_err_z.o
module_common_utils.o:
mt19937ar.o:
compute_err_z.o:


clean:
	$(RM) $(RM_LIST)
