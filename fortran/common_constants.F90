!-------------------------------------------------------------------------------
! Name: common_constants.F90
!-------------------------------------------------------------------------------
MODULE COMMON_CONSTANTS 

    IMPLICIT NONE 

    ! Type KIND value
    INTEGER, PARAMETER :: byte=1
    INTEGER, PARAMETER :: sint=2
    INTEGER, PARAMETER :: lint=4
    INTEGER, PARAMETER :: sREAL=4
    INTEGER, PARAMETER :: dREAL=8

    INTEGER, PARAMETER :: cmd_arg_length=128
    INTEGER, PARAMETER :: file_length=512
    INTEGER, PARAMETER :: path_length=2048

    INTEGER, PARAMETER :: date_length=4
    INTEGER, PARAMETER :: platform_length=8
    INTEGER, PARAMETER :: sensor_length=8

    INTEGER, PARAMETER :: attribute_length=128
    INTEGER, PARAMETER :: attribute_length_long=2048
    INTEGER, PARAMETER :: unitlength=128
    INTEGER, PARAMETER :: var_length=64
    INTEGER, PARAMETER :: var_name=100

    INTEGER, PARAMETER :: MAX_NC_NAME=256
    INTEGER, PARAMETER :: MAX_VAR_DIMS=32

    ! ORAC fill values
    INTEGER(KIND=byte),  PARAMETER :: byte_fill_value=-127
    INTEGER(KIND=sint),  PARAMETER :: sint_fill_value=-32767
    INTEGER(KIND=lint),  PARAMETER :: lint_fill_value=-32767
    REAL(KIND=sreal),    PARAMETER :: sreal_fill_value=-999.0
    REAL(KIND=dreal),    PARAMETER :: dreal_fill_value=-999.0

    ! Mathematical constants
    REAL(KIND=sreal),    PARAMETER :: pi=3.14159265
    REAL(KIND=sreal),    PARAMETER :: d2r=pi/180.0

    ! phase definition
    INTEGER(KIND=sint), PARAMETER :: liquid=0
    INTEGER(KIND=sint), PARAMETER :: ice=1

    ! 2d histogram definitions
    INTEGER(KIND=sint), PARAMETER :: n_hist_phase=2
    INTEGER(KIND=sint), PARAMETER :: n_hist_cot=14
    INTEGER(KIND=sint), PARAMETER :: n_hist_ctp=16
    ! 1d histogram definitions
    INTEGER(KIND=sint), PARAMETER :: n_cot_bins=13
    INTEGER(KIND=sint), PARAMETER :: n_ctp_bins=15
    INTEGER(KIND=sint), PARAMETER :: n_ctt_bins=16
    INTEGER(KIND=sint), PARAMETER :: n_cwp_bins=14
    INTEGER(KIND=sint), PARAMETER :: n_cer_bins=11 

END MODULE COMMON_CONSTANTS
