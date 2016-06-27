#!/usr/bin/env bash

# --- REQUIRED for mpmd.py !!!
#module unload cray-mpich atp
#module swap craype-network-aries craype-network-none 
#module switch cdo/1.6.1 cdo/1.6.4
#module load cray-netcdf
#module list

module switch cdo/1.6.1 cdo/1.6.4
module load cray-netcdf
module unload cray-mpich atp
module swap craype-network-aries craype-network-none
module list
