#
# [NOTE]
#       This is a makefile for the program that create
#       an rms.set of data.
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

RM_LIST= *.o rms.exe *~ *mod error.status
INCLUDE=../Registry
PROGRAM= rms.exe
#LDOPTION=

include ../configure.letkf

.f.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90 -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

OBJS=  module_common_utils.o rms.o 

all: $(PROGRAM)

rms.exe: $(OBJS)
	$(FC) -o $@ $(FCFLAGS) $(OBJS) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

rms.o: module_common_utils.o
module_common_utils.o:

clean:
	$(RM) $(RM_LIST)
