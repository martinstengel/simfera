#!/usr/bin/env bash

# --- path settings for git repository
REP="/perm/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator"
TMP="/scratch/ms/de/sf7/cschlund/SIMULATOR/cloud_simulator"

# --- final output directory
CRUN="test" 
# --- cot threshold
THV=0.15
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
ED=1

EXE="${REP}/fortran/run_test/test_cloud_simulator.x" 
SSTF="${REP}/aux/sst_era_interim_0.5_0.5.nc"
ITMP="${TMP}/input"
OTMP="${TMP}/output"

echo "${EXE} ${THV} ${SCOPS} ${OVERLAP} ${MPC} ${SSTF} ${ITMP} ${OTMP}/${CRUN}/${SY} ${SY} ${EY} ${SM} ${EM} ${SD} ${ED}"

${EXE} ${THV} ${SCOPS} ${OVERLAP} ${MPC} \
    ${SSTF} ${ITMP} \
    ${OTMP}/${CRUN}/${SY} \
    ${SY} ${EY} ${SM} ${EM} ${SD} ${ED}
