!-------------------------------------------------------------------------------
! Name: common_constants.F90
!
! Purpose:
! Define here data types, string lengths, constants etc.
!
! Description and Algorithm details:
! None
!
! Arguments:
! None
!
! History:
! 2014/08/15, GM: Original version.
! 2014/08/30, GM: Change integer fill values to be consistent with the main
!    processor.
! 2014/08/30, GM: Added pi and d2r.
! 2015/12/15, AP: Move IRho terms from ECP_constants.
!
! $Id: common_constants.F90 3844 2016-01-25 15:04:37Z gmcgarragh $
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module common_constants

   implicit none

   ! Type kind value
   integer, parameter :: byte=1
   integer, parameter :: sint=2
   integer, parameter :: lint=4
   integer, parameter :: sreal=4
   integer, parameter :: dreal=8

   integer, parameter :: cmd_arg_length=128
   integer, parameter :: file_length=512
   integer, parameter :: path_length=2048

   integer, parameter :: date_length=4
   integer, parameter :: platform_length=8
   integer, parameter :: sensor_length=8

   integer, parameter :: attribute_length=128
   integer, parameter :: attribute_length_long=2048
   integer, parameter :: unitlength=128
   integer, parameter :: var_length=64

   integer, parameter :: MAX_NC_NAME=256
   integer, parameter :: MAX_VAR_DIMS=32

   ! ORAC fill values
   integer(kind=byte),  parameter :: byte_fill_value=-127
   integer(kind=sint),  parameter :: sint_fill_value=-32767
   integer(kind=lint),  parameter :: lint_fill_value=-32767
   real(kind=sreal),    parameter :: sreal_fill_value=-999.0
   real(kind=dreal),    parameter :: dreal_fill_value=-999.0

   ! Mathematical constants
   real(kind=sreal),    parameter :: pi=3.14159265
   real(kind=sreal),    parameter :: d2r=pi/180.0

   ! phase definition
   INTEGER(KIND=sint), parameter :: liquid=0
   INTEGER(KIND=sint), parameter :: ice=1

   ! 2d histogram definitions
   integer(kind=sint), parameter :: n_hist_phase=2
   integer(kind=sint), parameter :: n_hist_cot=14
   integer(kind=sint), parameter :: n_hist_ctp=16
   ! 1d histogram definitions
   integer(kind=sint), parameter :: n_cot_bins=13
   integer(kind=sint), parameter :: n_ctp_bins=15
   integer(kind=sint), parameter :: n_ctt_bins=16
   integer(kind=sint), parameter :: n_cwp_bins=14
   integer(kind=sint), parameter :: n_cer_bins=11 

end module common_constants
