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

RM_LIST= bvortex.exe *.o bvortex.dat bvortex.txt wrfinput_d01 prep_obs.exe obs.dat
INCLUDE=./
PROGRAM= bvortex.exe prep_obs.exe 

include ../configure.letkf

.f.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90 -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

OBJS1=  bvortex.o  module_common_solver.o    

OBJS2=  prep_obs.o


all: $(PROGRAM)

bvortex.exe: $(OBJS1)
	$(FC) -o $@ $(FCFLAGS) $(OBJS1) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

prep_obs.exe: ${OBJS2}
	$(FC) -o $@ $(FCFLAGS) $(OBJS2) -I$(NETCDF)/include/ $(NETCDF)/lib/libnetcdf.a

bvortex.o: module_common_solver.o
module_common_solver.o:
prep_obs.o:

clean:
	$(RM) $(RM_LIST)

