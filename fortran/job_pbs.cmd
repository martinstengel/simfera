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
#PBS -o /perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/log/20160627_cov2d.out
#PBS -e /perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator/fortran/log/20160627_cov2d.err

set -x

# --- path settings for git repository
REP="/perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator"
TMP="/scratch/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator"

# --- final output directory
CRUN="v6.0_via_cmd/5_cov2d" 
# --- cot threshold
THV=0.001
# --- DWDscops=1, COSPscops=2
SCOPS=1
# --- max=1, random=2, max_random=3
OVERLAP=3
# --- no_mixed_phase=1, mixed_phase=2
MPC=2
# --- start year
SY=2008
# --- end year
EY=2008
# --- start month
SM=7
# --- end month
EM=7
# --- start day
SD=1
# --- end day
ED=0

EXE="${REP}/fortran/cloud_simulator.x" 
SSTF="${REP}/aux/sst_era_interim_0.5_0.5.nc"
ITMP="${TMP}/input"
OTMP="${TMP}/output"

${EXE} ${THV} ${SCOPS} ${OVERLAP} ${MPC} ${SSTF} ${ITMP} ${OTMP}/${CRUN}/${SY} ${SY} ${EY} ${SM} ${EM} ${SD} ${ED}
