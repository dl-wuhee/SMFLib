#Python script for automatically generate makefile for fortran source files
#which are all in the same directory
#
#version: 1.7
#fix date: 2013/12/22
#
#All copyrights reserved by Dan Li @ Wuhan University
#Please contact danlee@whu.edu.cn if you have any suggestion
#

#Generated at 2022-05-23 14:13:52

#HDF5HOME=/path/to/hdf5
#HDF5INC=$(HDF5HOME)/include
#HDF5LIB=$(HDF5HOME)/lib
#HDF5LIBS=-lhdf5 -L$(HDF5LIB)
#BLASLIBS=-lblas -L/path/to/blas
#LAPACKLIBS=-llapack -L/path/to/lapack
#MPILIBS=-lmpi -L/path/to/mpi
 
USEDEFAULTHOME=./
USERDEFAULTMODDIR=./
USERDEFAULTLIBDIR=./
USERDEFAULTLIBS=

#USEDEFAULTHOME=./test
#USERDEFAULTMODDIR=$(USEDEFAULTHOME)/include
#USERDEFAULTLIBDIR=$(USEDEFAULTHOME)/lib
#USERDEFAULTLIBS=-la

FC=gfortran

mode=debug
ifeq ($(mode), debug)
BUILDDIR=./../build/debug
FFFLAGS=-std=gnu -Wall -fbounds-check -g -Ddebug -fopenmp
#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)
#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check
LDFLAGS=-fopenmp -L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)
#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)
else
BUILDDIR=./../build/release
FFFLAGS=-std=gnu -Wall -fbounds-check -fopenmp
#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)
#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check
LDFLAGS=-fopenmp -O3 -L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)
#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)
endif
OBJOUTDIR=$(BUILDDIR)/obj
MODOUTDIR=$(BUILDDIR)/mod
BINOUTDIR=$(BUILDDIR)/bin
LIBOUTDIR=$(BUILDDIR)/lib

ifeq ($(OS), Window_NT)
PROGRAM=main_$(mode).exe
else
PROGRAM=main_$(mode)
endif



SOURCES=sparsematrix.f90 simple_precision.f90 array.f90

OBJS=$(OBJOUTDIR)/sparsematrix.o $(OBJOUTDIR)/simple_precision.o $(OBJOUTDIR)/array.o


$(PROGRAM): $(OBJS)
	$(FC) -o $(BINOUTDIR)/$(PROGRAM) $^ $(LDFLAGS)

$(OBJOUTDIR)/sparsematrix.o: sparsematrix.f90 $(OBJOUTDIR)/array.o $(OBJOUTDIR)/simple_precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/simple_precision.o: simple_precision.f90 
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@

$(OBJOUTDIR)/array.o: array.f90 $(OBJOUTDIR)/simple_precision.o
	$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR)  -o $@


$(OBJS): | $(BUILDDIR)

$(BUILDDIR): 
	mkdir -p $(BUILDDIR)/{bin,lib,obj,mod}

.PHONY: clean veryclean

clean:
	rm -f $(OBJOUTDIR)/*.o $(MODOUTDIR)/*.mod $(MODOUTDIR)/*.MOD $(LIBOUTDIR)/*.lib
veryclean: clean
	rm -f $(BINOUTDIR)/$(PROGRAM)