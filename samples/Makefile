
GCC = /usr/bin/gcc

# Linux
#XORGDIR = /usr/lib64/xorg/modules
#GCCDIR = /usr/lib/gcc/x86_64-redhat-linux/4.8.5
LIB64 = /usr/lib64
#GFORTRAN = /usr/bin/gfortran

LNXLIB = -L${LIB64} -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common
#LNXLIB = -L${XORGDIR} -L${LIB64} -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common -lpng #-Wno-comment -Wno-long-long ## (only for C/C++ but not for f90)
LNXOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${LNXLIB}" ALL="${ALL}"

# Mac
#XORGDIR =/opt/X11/lib
MACLIB = -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common
#MACLIB = -L${XORGDIR} -lreadline -lX11 -lgcc -ltermcap -ldl -lm -lc -Dunix -DPG_PPU -g -Wall -fno-automatic -fPIC -fno-common #-Wno-comment -Wno-long-long 
MACOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" LINKLIB="${MACLIB}" ALL="${ALL}"

ALL = sample01

start:
	$(MAKE) `uname`

## Required to remake ./*.mod (though it seems unnecessary in macOS...)
sample01.o: sample01.f90 ${ASMTEST_OBJ}
	${FC} ${FFLAGS} -I../src -c -o $@ $<

sample01: sample01.o ../src/libalpinf90utils.a
	${LD} -I../src -o $@ $< -L../src -lalpinf90utils ${LINKLIB}
# NOTE: The order of "-L" and "-l" matter in Linux (but not in macOS)!!  In Linux, "-L" must *follow* the object files.


.SUFFIXES: .f90 .mod

#%.o %.mod: %.f90
#%.o: %.f90
#.f90.o:
%.o: %.f90
	${FC} ${FFLAGS} -I../src -c -o $@ $<

#.f.o:
#	${FC} ${FFLAGS} -c -o $@ $<
#.c.o:
#	${CC} ${CFLAGS} -c -o $@ $<

all: ${ALL}


.PHONY: test clean Linux Darwin

Linux:
	${MAKE} ${LNXOPT} all

Darwin:
	${MAKE} ${MACOPT} all

run::
	make && (echo "% ./sample01 dummy This must FAIL."; ./sample01 dummy This must FAIL. ; echo "% ./sample01 -v dummy2"; ./sample01 -v dummy2)

clean::
	-${RM} -f *.o *.mod ${ALL}

