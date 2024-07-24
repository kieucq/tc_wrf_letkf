SRCS = dmpbfr.f ufbdmp.f
FFLAGS	= 
LIBS	= -L/state/partition1/home/kieuc/opt/bufr -lbufr
dmpbfr.exe : $(SRCS)
	export XLFRTEOPTS="unit_vars=yes"
	pgf90 $(FFLAGS) -o dmpbfr.exe $(SRCS) $(LIBS)
