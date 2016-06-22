#
# Makefile for cloud simulator
#

# The compiler: CRAY = crayftn (through ftn)
FC = ftn

# flags for debugging or for maximum performance, 
#FCFLAGS = -O3 -m0 -K trap=fp -R b -hbyteswapio -N 132
#FCFLAGS = -O3 -K trap=fp -R b -hbyteswapio -N 132
FCFLAGS = -O3 -K trap=fp -R b -N 132
#FCFLAGS += -I/usr/include -Iobj
FCFLAGS += -Iobj
# ==============================================================
# Objects 
# ==============================================================

MAIN = cloud_simulator

# Tools
T1 = calender
T2 = funcs
T3 = subs

# from_cosp
C1 = cosp_kinds
C2 = mo_rng
C3 = cosp_errorHandling
C4 = scops

# simulator
S1 = common_constants
S2 = ncdf_constants
S3 = structs
S4 = initialize
S5 = undefine
S6 = sim_ncdf
S7 = sim_core


# Objects
# Make sure these are in the right order
OBJ = obj/$(S1).o\
	  obj/$(S2).o\
	  obj/$(T1).o\
	  obj/$(T2).o\
	  obj/$(S3).o\
	  obj/$(S4).o\
	  obj/$(S5).o\
	  obj/$(S6).o\
	  obj/$(T3).o\
	  obj/$(C1).o\
	  obj/$(C2).o\
	  obj/$(C3).o\
	  obj/$(C4).o\
	  obj/$(S7).o\
	  obj/$(MAIN).o

# RULES
ALL: $(MAIN).x
#
$(MAIN).x: $(OBJ)
	$(FC) $(FCFLAGS) $(OBJ) -o $(MAIN).x

# Tools
obj/$(T1).o: src/tools/$(T1).F90 
	$(FC) $(FCFLAGS) -c -o obj/$(T1).o src/tools/$(T1).F90
obj/$(T2).o: src/tools/$(T2).F90
	$(FC) $(FCFLAGS) -c -o obj/$(T2).o src/tools/$(T2).F90
obj/$(T3).o: src/tools/$(T3).F90
	$(FC) $(FCFLAGS) -c -o obj/$(T3).o src/tools/$(T3).F90

# from_cosp
obj/$(C1).o: src/from_cosp/$(C1).F90
	$(FC) $(FCFLAGS) -c -o obj/$(C1).o src/from_cosp/$(C1).F90
obj/$(C2).o: src/from_cosp/$(C2).F90
	$(FC) $(FCFLAGS) -c -o obj/$(C2).o src/from_cosp/$(C2).F90
obj/$(C3).o: src/from_cosp/$(C3).F90
	$(FC) $(FCFLAGS) -c -o obj/$(C3).o src/from_cosp/$(C3).F90
obj/$(C4).o: src/from_cosp/$(C4).F90
	$(FC) $(FCFLAGS) -c -o obj/$(C4).o src/from_cosp/$(C4).F90

# simulator
obj/$(S1).o: src/sim/$(S1).F90
	@mkdir -p obj 
	$(FC) $(FCFLAGS) -c -o obj/$(S1).o src/sim/$(S1).F90
obj/$(S2).o: src/sim/$(S2).F90
	$(FC) $(FCFLAGS) -c -o obj/$(S2).o src/sim/$(S2).F90
obj/$(S3).o: src/sim/$(S3).F90
	$(FC) $(FCFLAGS) -c -o obj/$(S3).o src/sim/$(S3).F90
obj/$(S4).o: src/sim/$(S4).F90
	$(FC) $(FCFLAGS) -c -o obj/$(S4).o src/sim/$(S4).F90
obj/$(S5).o: src/sim/$(S5).F90
	$(FC) $(FCFLAGS) -c -o obj/$(S5).o src/sim/$(S5).F90
obj/$(S6).o: src/sim/$(S6).F90
	$(FC) $(FCFLAGS) -c -o obj/$(S6).o src/sim/$(S6).F90
obj/$(S7).o: src/sim/$(S7).F90
	$(FC) $(FCFLAGS) -c -o obj/$(S7).o src/sim/$(S7).F90

# MAIN program
obj/$(MAIN).o: src/$(MAIN).F90
	$(FC) $(FCFLAGS) -c -o obj/$(MAIN).o src/$(MAIN).F90

clean:
	rm -rf obj *.x

