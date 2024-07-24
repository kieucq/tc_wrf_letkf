#
#

include ../../configure.gsi

all: ssrc.exe

ssrc.exe: ssrc.c
	$(CC) $(CFLAGS) $< -o $@
	$(CP) $@ $(COREDIR)/run

clean:
	-/bin/rm -f *.o *.exe
