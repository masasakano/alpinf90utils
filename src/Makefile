
GCC = /usr/bin/gcc

# Linux
#GCCDIR = /usr/lib/gcc/x86_64-redhat-linux/4.8.5
LIB64 = /usr/lib64
#GFORTRAN = /usr/bin/gfortran
LNXOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" ALL="${ALL}"

# Mac
MACOPT = LD="gfortran -g" CC="${GCC} -g" FC="gfortran -g -c -fno-second-underscore -ff2c" ALL="${ALL}"

ALL = libalpinf90utils.a alpin_unittest.o

start:
	$(MAKE) `uname`

#alpin_err_exit.o: alpin_err_exit.f90
#	${FC} ${FFLAGS} -c -o $@ $<

### NOTE: The following may be necessary if you modify function/subroutine interfaces in the parent source(s).
#alpin_misc_utils.o: alpin_misc_utils.f90 alpin_err_exit.o
#	${FC} ${FFLAGS} -c -o $@ $<
#
#alpin_hash.o: alpin_hash.f90 alpin_misc_utils.o
#	${FC} ${FFLAGS} -c -o $@ $<

alpin_unittest.o: alpin_unittest.f90 alpin_misc_utils.o alpin_err_exit.o
	${FC} ${FFLAGS} -c -o $@ $<

libalpinf90utils.a: alpin_err_exit.o alpin_misc_utils.o alpin_hash.o
	ar -rsv $@ $^

.SUFFIXES: .f90 .mod

#.f90.o:   # This does not recompile when .f90 is updated for some reason.
#%.o %.mod: %.f90
%.o: %.f90
	${FC} ${FFLAGS} -c -o $@ $<

#.f.o:
#	${FC} ${FFLAGS} -c -o $@ $<
#.c.o:
#	${CC} ${CFLAGS} -c -o $@ $<

all: ${ALL}


.PHONY: test clean Linux Darwin #install

Linux:
	${MAKE} ${LNXOPT} all

Darwin:
	${MAKE} ${MACOPT} all

#INSTALL_DIR = ../bin
#install:
#	if [ ! -d ${INSTALL_DIR} ]; then mkdir -p ${INSTALL_DIR}; fi
#	mv -f ${ALL} ${INSTALL_DIR}

test::
	make && [ -x ../test ] && (cd ../test && make test)

clean::
	-${RM} -f *.o *.mod ${ALL}

