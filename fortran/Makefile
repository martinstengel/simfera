#
# Makefile for cloud simulator
#
# The compiler
FC = ftn
# flags for debugging or for maximum performance, comment as necessary
FCFLAGS = -g -fbounds-check
FCFLAGS = -O2
#FCFLAGS = -O3
# flags forall (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include

# libraries needed for linking, unused in the examples
#LDFLAGS = -li_need_this_lib

objects = common_constants.o funcs.o structs.o subs.o cloud_simulator.o

cloud_simulator: $(objects)
	$(FC) -o cloud_simulator $(objects)

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ $(PROGRAMS)

