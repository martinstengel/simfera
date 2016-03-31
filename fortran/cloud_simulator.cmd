#!/usr/bin/env bash

#PBS -N cloud_simulator
#PBS -q ns
#PBS -m e
#PBS -M dec4@ecmwf.int
#PBS -l walltime=24:00:00
#PBS -l EC_ecfs=0
#PBS -l EC_mars=0
#PBS -l EC_total_tasks=1
#PBS -l EC_threads_per_task=1
#PBS -l EC_memory_per_task=3000mb
#PBS -o /perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/log/20160331_2_cloud_simulator.out
#PBS -e /perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/log/20160331_2_cloud_simulator.err

set -x

THV=0.3
MPC=2
SCOPS=2
CRUN="v5.9_fortran"
PERM="/perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator"
ITMP="/scratch/ms/de/dec4/SIMULATOR/cloud_simulator/input"
OTMP="/scratch/ms/de/dec4/SIMULATOR/cloud_simulator/output"
SY=2008
EY=2008
SM=7
EM=7
SD=1
ED=0

EXE="/perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/cloud_simulator" 

${EXE} ${THV} ${MPC} ${SCOPS} ${CRUN} ${PERM} ${ITMP} {OTMP} ${SY} ${EY} ${SM} ${EM} ${SD} ${ED}

