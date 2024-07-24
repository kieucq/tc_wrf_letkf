#
# [NOTE]
#       This is a makefile for the program that create
#       a set of truth data.
#       truth.exe: plotting program for checking truth
#       bvortex.exe: program for planing a bogus vortex
#       into the initial condition
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

RM_LIST= *.o tru_check.exe *~ *mod error.status bvortex.exe tru_check.dat
INCLUDE=../Registry
PROGRAM= tru_check.exe bvortex.exe ideal2real.exe
#LDOPTION=

include ../configure.letkf

.f.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90 -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

OBJS1=  bvortex.o  module_common_solver.o    

OBJS2=  tru_check.o  module_common_utils.o 

OBJS3=  ideal2real.o

all: $(PROGRAM)

tru_check.exe: $(OBJS2)
	$(FC) -o $@ $(FCFLAGS) $(OBJS2) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

bvortex.exe: $(OBJS1)
	$(FC) -o $@ $(FCFLAGS) $(OBJS1) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a	

ideal2real.exe: $(OBJS3)
	$(FC) -o $@ $(FCFLAGS) $(OBJS3) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

bvortex.o: module_common_solver.o
module_common_utils.o:
tru_check.o: module_common_utils.o
module_common_solver.o:
ideal2real.o:

clean:
	$(RM) $(RM_LIST)
