#!/usr/bin/env bash

module switch cdo cdo/1.6.4
module switch cray-netcdf netcdf4
module swap craype-network-aries craype-network-none
# cray-hdf5 verlinkt libpmi.so.0 und mehr
module unload cray-hdf5
module unload cray-mpich
module unload atp
module unload pmi
module list
# make clean
# make
