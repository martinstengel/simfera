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

src1 = common_constants.o calender.o funcs.o structs.o
src2 = initialize.o undefine.o sim_ncdf.o subs.o sim_core.o
main = cloud_simulator.o
objects = $(src1) $(src2) $(main)

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

