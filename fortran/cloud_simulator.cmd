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
#PBS -o /perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/log/20160405_cloud_simulator.out
#PBS -e /perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/log/20160405_cloud_simulator.err

set -x

THV=0.3
MPC=2
SCOPS=2
CRUN="test2"
SSTF="/perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/aux/sst_era_interim_0.5_0.5.nc"
ITMP="/scratch/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/input"
OTMP="/scratch/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/output"
SY=2008
EY=2008
SM=7
EM=7
SD=1
ED=0

EXE="/perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/cloud_simulator" 

${EXE} ${THV} ${MPC} ${SCOPS} ${SSTF} ${ITMP} ${OTMP}/${CRUN}/${SY} ${SY} ${EY} ${SM} ${EM} ${SD} ${ED}

