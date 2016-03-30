#
# Makefile for cloud simulator
#
# The compiler: CRAY = crayftn (through ftn)
FC = ftn

# flags for debugging or for maximum performance, comment as necessary
FCFLAGS = -O3 -K trap=fp -R b -hbyteswapio -N 132
FCFLAGS += -I/usr/include

# all modules
PROG = common_constants.o ncdf_constants.o calender.o funcs.o structs.o
PROG += initialize.o undefine.o sim_ncdf.o subs.o sim_core.o
# main program
MAIN = cloud_simulator.o
OBJS = $(PROG) $(MAIN)

cloud_simulator: $(OBJS)
	$(FC) -o cloud_simulator $(OBJS)

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -f *.o *.mod *.MOD
