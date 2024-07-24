include ../configure.letkf
# compaq flags

LIBNETCDF = -L${NETCDF}/lib -lnetcdf -lm
INCLUDE = -I${NETCDF}/include -I./
FC = ifort
FCFLAGS =  -free 
CPP = cpp
CPPFLAGS = -I. -P -Ddunix -Ddcenter  -Ddgmean

# linux flags

#LIBNETCDF = -L${NETCDF}/lib -lnetcdf -lm
#INCLUDE = -I${NETCDF}/include -I./
#FC = pgf90
#FCFLAGS = -g -C -Mfree 
#FCFLAGS = -fast -Mfree 
#CPP = cpp
#CPPFLAGS = -I. -C -traditional -Ddlinux -Ddcenter -Ddgmean
##CPPFLAGS = -I. -C -traditional -Ddlinux -Ddcenter  -Dmove_left

# Sun flags

#LIBNETCDF = -L/usr/local/netcdf/lib -lnetcdf -lm
#INCLUDE = -I/usr/local/netcdf/include -I./
#FC = f90
#FCFLAGS = -g -C -free 
#CPP = /usr/ccs/lib/cpp
#CPPFLAGS = -I. -C -P -Ddirect -Dbytesw

# SGI flags

#LIBNETCDF = -L/usr/local/netcdf/lib -lnetcdf -lm
#INCLUDE = -I/usr/local/netcdf/include -I./
#FC = f90
#FCFLAGS = -g -C -freeform
#CPP = /lib/cpp
#CPPFLAGS = -I. -C -P -Ddirect -Dbytesw

# IBM flags

#LIBNETCDF = -L/usr/local/lib32/r4i4 -lnetcdf -lm
#INCLUDE = -I/usr/local/include -I./
#FC = xlf
#FCFLAGS = -g -C -qfree=f90                
#CPP = /usr/lib/cpp
#CPPFLAGS = -I. -C -P -Ddirect 
PROGRAM=wrf_to_grads tc_ensemble_wpost_tseries.exe
 
.f.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90 -I$(NETCDF)/include $(NETCDF)/lib/libnetcdf.a

all: $(PROGRAM) 

wrf_to_grads: wrf_to_grads.o module_wrf_to_grads_netcdf.o module_wrf_to_grads_util.o
	$(FC) $(FCFLAGS) -o wrf_to_grads wrf_to_grads.o \
                         module_wrf_to_grads_netcdf.o \
                         module_wrf_to_grads_util.o \
                         $(LIBNETCDF) 

wrf_to_grads.o: wrf_to_grads.F module_wrf_to_grads_util.o
	$(CPP) $(CPPFLAGS) wrf_to_grads.F > wrf_to_grads.f
	$(FC) $(FCFLAGS) -c wrf_to_grads.f $(INCLUDE)

module_wrf_to_grads_util.o: module_wrf_to_grads_util.F module_wrf_to_grads_netcdf.o 
	$(CPP) $(CPPFLAGS) module_wrf_to_grads_util.F > module_wrf_to_grads_util.f
	cp module_wrf_to_grads_util.f.fixed module_wrf_to_grads_util.f
	$(FC) $(FCFLAGS) -c module_wrf_to_grads_util.f $(INCLUDE) 

module_wrf_to_grads_netcdf.o: module_wrf_to_grads_netcdf.F
	$(CPP) $(CPPFLAGS) module_wrf_to_grads_netcdf.F > module_wrf_to_grads_netcdf.f
	$(FC) $(FCFLAGS) -c module_wrf_to_grads_netcdf.f $(INCLUDE)

tc_ensemble_wpost_tseries.exe: tc_ensemble_wpost_tseries.o
	$(FC) -o tc_ensemble_wpost_tseries.exe tc_ensemble_wpost_tseries.o

tc_ensemble_wpost_tseries.o: tc_ensemble_wpost_tseries.f90
	$(FC) $(FCFLAGS) -c tc_ensemble_wpost_tseries.f90

clean:	
	rm -f *.o *.mod *.f wrf_to_grads
