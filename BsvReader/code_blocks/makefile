# gnumake makefile for compiling BsvReader with gfortran.
#
PROGRAM = BsvReader
SOURCEDIR1=../src
SOURCEDIR2=../src/Utilities
SOURCEDIR3=../src/Utilities/Observation
OBJDIR = ./Debug/obj
#MODDIR = ./Debug/mod
EXEDIR = ./Debug/bin

#VPATH = $(SOURCEDIR1) $(SOURCEDIR2) $(SOURCEDIR3)
VPATH = $(SOURCEDIR1) $(SOURCEDIR2) $(SOURCEDIR3)

.SUFFIXES:
.SUFFIXES: .c .f .f90 .F90 .o

# Define the Fortran compile flags

F90FLAGS =
#
# gfortran flags
F90FLAGS=-g -ggdb -fcheck=all -ffpe-trap=invalid,zero,overflow

F90 = gfortran

# Define the C compile flags
# -D_UF defines UNIX naming conventions for mixed language compilation.
#
CFLAGS = -D_UF -O3
CC = gcc

# Define the libraries

#SYSLIBS= -lc
SYSLIBS =
USRLIB  =

# Define all Fortran object files which make up mf5to6

OBJECTS = \
$(OBJDIR)/kind.o \
$(OBJDIR)/OpenSpec.o \
$(OBJDIR)/Constants.o \
$(OBJDIR)/List.o \
$(OBJDIR)/SimVariables.o \
$(OBJDIR)/ArrayHandlers.o \
$(OBJDIR)/Sim.o \
$(OBJDIR)/InputOutput.o \
$(OBJDIR)/ObsOutput.o \
$(OBJDIR)/readers.o \
$(OBJDIR)/BsvReader.o

# Define Task Function

all : BsvReader

# Define

# the -I option tells gfortran where to find .mod files
# -- could use $(MODDIR) instead of $(OBJDIR).

BsvReader : $(OBJECTS)
	$(F90) $(F90FLAGS) -o $(EXEDIR)/BsvReader $(OBJECTS) $(USRLIB) $(SYSLIBS) -I$(OBJDIR)

# Object codes

# Automatic variables:
# $@ The file name of the target of the rule
# $< The name of the first prerequisite
# $^ The names of all the prerequisites, with spaces between them

# Pattern Rules
# Form:
# target : prerequisite
#	recipe

# the -J option tells gfortran where to put .mod files
# -- could use $(MODDIR) instead of $(OBJDIR).

$(OBJDIR)/%.o : %.f90
	$(F90) $(F90FLAGS) -c $< -o $@ -J$(OBJDIR)

$(OBJDIR)/%.o : %.f
	$(F90) $(F90FLAGS) -c $< -o $@ -J$(OBJDIR)

.PHONY : clean
clean :
	-rm $(EXE) $(OBJDIR)/*.o $(OBJDIR)/*.mod

#  end

