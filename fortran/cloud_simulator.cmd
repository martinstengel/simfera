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
#PBS -l EC_memory_per_task=1024mb
#PBS -o /perm/ms/de/sf7/cschlund/cloud_simulator/fortran/log_cloud_simulator.out
#PBS -e /perm/ms/de/sf7/cschlund/cloud_simulator/fortran/log_cloud_simulator.err

set -x
/perm/ms/de/sf7/cschlund/cloud_simulator/fortran/cloud_simulator /perm/ms/de/sf7/cschlund/cloud_simulator/fortran/config.file

