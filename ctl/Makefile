#
# [NOTE]
#       This is a makefile for the program that check the 
#       control run.
#
# [HISTORY]
#     - 27 Mar 2010: created by Chanh Kieu
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

RM_LIST= *.o ctl_check.exe *~ *mod error_status ctl_check.dat
INCLUDE=../Registry
PROGRAM= ctl_check.exe
#LDOPTION=

include ../configure.letkf

.f.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90 -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

OBJS=  ctl_check.o  module_common_utils.o 

all: $(PROGRAM)

ctl_check.exe: $(OBJS)
	$(FC) -o $@ $(FCFLAGS) $(OBJS) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a


module_common_utils.o:
ctl_check.o: module_common_utils.o

clean:
	$(RM) $(RM_LIST)
