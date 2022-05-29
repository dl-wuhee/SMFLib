#This version is not suitable to use for some condtion
import os
import os.path
import glob
import re
from datetime import datetime


def formatoutput( mfp, sour, nele, types ):
    values = {1:["SOURCES=", "        " ], 2:["OBJS=","     "]}
    if (types == 2):
        sour = [ "$(OBJOUTDIR)/" + s for s in sour]
    if (len(sour) > nele):
        mfp.write(values[types][0] + " ".join(sour[0:nele]) + "\\\n")
        s = 0
        strnotend = True
        while strnotend:
            s = s + nele
            if (s > len(sour)):
                strnotend = False
            else:
                e = s + nele
                if (e > len(sour)):
                    e = len(sour)
                    mfp.write(values[types][1]+ " ".join(sour[s:e]) + "\n")
                else:
                    mfp.write(values[types][1] + " ".join(sour[s:e]) + "\\\n")
    else:
        mfp.write(values[types][0] + " ".join(sour)+"\n")
    mfp.write("\n")


sources = [] 
for extstr in ['*.f90','*.f95', 'f03', '*.for', '*.f', '*.F90', '*.F95', 'F03', '*.FOR', '*.F', '*.For']:
    sources.extend(glob.glob(extstr))

if (sources):
    info = []
    modinfo = {}

    sources = list(set(sources))
    obj = []
    for sf in sources:
        sfo = os.path.splitext(sf)[0] + ".o"
        obj.append(sfo)
        sfc = re.sub(re.compile('!.*'), '', ''.join(open(sf, 'r').readlines())).lower()
        sfmod = list(set([re.sub('\n','', mod).strip('\ ').split(' ')[1] for mod in re.findall('\s*module\ +\w+', sfc)]))
        for mod in sfmod:
            modinfo[mod] = sfo
        uselist = list(set([re.sub('\n','', use).strip('\ ').split(' ')[1] for use in re.findall('\s*use\ +\w+', sfc)]))
        info.append([sfo,sf,uselist])

    print(info)
    print(modinfo)

    deps = {}
    extincs = {}
    for inf in info:
        dep = []
        extinc = []
        print(inf[1])
        for ul in inf[2]:
            if (ul in modinfo.keys()):
                dep.append(modinfo[ul])
            else:
                extinc.append("-I$(" + ul.upper() + "_DIR)")
            dep = [ d for d in list(set(dep)) if d != inf[0] ]
        dep = ["$(OBJOUTDIR)/" + d for d in dep]
        deps[inf[0]] = (inf[1], dep)
        extincs[inf[0]] = (inf[1], extinc)

    mf = 'Makefile'
    mfp = open(mf, 'w')


    copyright = [
            "#Python script for automatically generate makefile for fortran source files",
            "#which are all in the same directory",
            "#",
            "#version: 1.7",
            "#fix date: 2013/12/22",
            "#",
            "#All copyrights reserved by Dan Li @ Wuhan University",
            "#Please contact danlee@whu.edu.cn if you have any suggestion",
            "#",
            "",
            "#Generated at "+str(datetime.now())[0:19] 
            ]

    head = [
            "#HDF5HOME=/path/to/hdf5",
            "#HDF5INC=$(HDF5HOME)/include",
            "#HDF5LIB=$(HDF5HOME)/lib",
            "#HDF5LIBS=-lhdf5 -L$(HDF5LIB)",
            "#BLASLIBS=-lblas -L/path/to/blas",
            "#LAPACKLIBS=-llapack -L/path/to/lapack",
            "#MPILIBS=-lmpi -L/path/to/mpi",
            " ",
            "USEDEFAULTHOME=./",
            "USERDEFAULTMODDIR=./",
            "USERDEFAULTLIBDIR=./",
            "USERDEFAULTLIBS=",
            "",
            "#USEDEFAULTHOME=./test",
            "#USERDEFAULTMODDIR=$(USEDEFAULTHOME)/include",
            "#USERDEFAULTLIBDIR=$(USEDEFAULTHOME)/lib",
            "#USERDEFAULTLIBS=-la",
            "",
            "FC=gfortran",
            "",
            "mode=debug",
            "ifeq ($(mode), debug)",
            "BUILDDIR=./../build/debug",
            "FFFLAGS=-std=gnu -Wall -fbounds-check -g -Ddebug -fopenmp",
            "#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)",
            "#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check",
            "LDFLAGS=-fopenmp -L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)",
            "#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)",
            "else",
            "BUILDDIR=./../build/release",
            "FFFLAGS=-std=gnu -Wall -fbounds-check -fopenmp",
            "#FFFLAGS=-std=gnu -Wall -fbounds-check -I$(HDF5INC)",
            "#FFFLAGS=-std=gnu -ffree-form -Wall -g -fbounds-check",
            "LDFLAGS=-fopenmp -O3 -L$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS)",
            "#LDFLAGS=$(USERDEFAULTLIBDIR) $(USERDEFAULTLIBS) $(BLASLIBS) $(LAPACKLIBS) $(MPILIBS) $(HDF5LIBS)",
            "endif",
            "OBJOUTDIR=$(BUILDDIR)/obj",
            "MODOUTDIR=$(BUILDDIR)/mod",
            "BINOUTDIR=$(BUILDDIR)/bin",
            "LIBOUTDIR=$(BUILDDIR)/lib",
            "", 
            "ifeq ($(OS), Window_NT)",
            "PROGRAM=main_$(mode).exe",
            "else",
            "PROGRAM=main_$(mode)",
            "endif"
            ]

    newext = []
    for k in extincs:
        newext.extend(extincs[k][1])
    newext = [ne[4:-1] + "=$(USERDEFAULTMODDIR)\n" + "#" + ne[4:-1] + "=$(HDF5INC)" for ne in newext]
    print(newext)

    tail = [
            ".PHONY: clean veryclean",
            "",
            "clean:",
            "	rm -f $(OBJOUTDIR)/*.o $(MODOUTDIR)/*.mod $(MODOUTDIR)/*.MOD $(LIBOUTDIR)/*.lib",
            "veryclean: clean",
            "	rm -f $(BINOUTDIR)/$(PROGRAM)"
            ]


    mfp.write("\n".join(copyright)+"\n\n")
    mfp.write("\n".join(head)+"\n\n")
    mfp.write("=\n".join(newext)+"\n\n")
    formatoutput(mfp, sources, 7, 1)
    formatoutput(mfp, obj, 7, 2)


    mfp.write("\n")
    mfp.write("$(PROGRAM): $(OBJS)\n")
    mfp.write("\t$(FC) -o $(BINOUTDIR)/$(PROGRAM) $^ $(LDFLAGS)\n\n")



    for inf in info:
        mfp.write("$(OBJOUTDIR)/"+inf[0]+": "+inf[1] +' ' +' '.join(deps[inf[0]][1])+"\n")
        mfp.write("\t$(FC) $(FFFLAGS) -c $< -J$(MODOUTDIR) " + ' '.join(extincs[inf[0]][1]) + " -o $@\n\n")

    mfp.write("\n")
    mfp.write("$(OBJS): | $(BUILDDIR)\n")
    mfp.write("\n")
    mfp.write("$(BUILDDIR): \n")
    mfp.write("\tmkdir -p $(BUILDDIR)/{bin,lib,obj,mod}\n\n")

    mfp.write("\n".join(tail))
    mfp.close()
