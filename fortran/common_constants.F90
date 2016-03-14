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
    REAL(KIND=sreal), PARAMETER :: pi=3.14159265
    REAL(KIND=sreal), PARAMETER :: d2r=pi/180.0
    REAL(KIND=sreal), PARAMETER :: r_specific=287.058 ![J/(kg*K)]
    REAL(KIND=sreal), PARAMETER :: zd_sea=0.33
    REAL(KIND=sreal), PARAMETER :: zd_land=0.43
    REAL(KIND=sreal), PARAMETER :: zntot_sea=100.
    REAL(KIND=sreal), PARAMETER :: zntot_land=300.
    REAL(KIND=sreal), PARAMETER :: rtt=273.15
    REAL(KIND=sreal), PARAMETER :: zrefde=0.64952
    REAL(KIND=sreal), PARAMETER :: rho_water=1000. ![kg/m3]
    REAL(KIND=sreal), PARAMETER :: rho_ice=916.7   ![kg/m3]
    REAL(KIND=sreal), PARAMETER :: qext_water=2.0
    REAL(KIND=sreal), PARAMETER :: qext_ice=2.1

    ! land sea mask
    INTEGER(KIND=sint), PARAMETER :: sea=0
    INTEGER(KIND=sint), PARAMETER :: land=1

    ! sim_core.F90
    INTEGER(KIND=sint), PARAMETER :: is_night=0
    INTEGER(KIND=sint), PARAMETER :: is_day=1
    INTEGER(KIND=sint), PARAMETER :: no_mixed_phase=1
    INTEGER(KIND=sint), PARAMETER :: mixed_phase=2
    INTEGER(KIND=sint), PARAMETER :: rand=1
    INTEGER(KIND=sint), PARAMETER :: max_rand=2
    INTEGER(KIND=sint), PARAMETER :: max_cot=100.0
    REAL(KIND=sreal),   PARAMETER :: is_clear=0.0
    REAL(KIND=sreal),   PARAMETER :: is_cloud=1.0
    REAL(KIND=sreal),   PARAMETER :: is_ice=0.0
    REAL(KIND=sreal),   PARAMETER :: is_liq=1.0

    ! GET_MEAN in funcs.F90
    INTEGER(KIND=sint), PARAMETER :: normal=0
    INTEGER(KIND=sint), PARAMETER :: allsky=1

    ! histogram definitions
    INTEGER(KIND=sint), PARAMETER :: n_hist_phase=2
    INTEGER(KIND=sint), PARAMETER :: liq_bin=1
    INTEGER(KIND=sint), PARAMETER :: ice_bin=2
    ! 2d histogram definitions
    INTEGER(KIND=sint), PARAMETER :: n_hist_cot=14
    INTEGER(KIND=sint), PARAMETER :: n_hist_ctp=16
    ! 1d histogram definitions
    INTEGER(KIND=sint), PARAMETER :: n_cot_bins=14
    INTEGER(KIND=sint), PARAMETER :: n_ctp_bins=15
    INTEGER(KIND=sint), PARAMETER :: n_ctt_bins=16
    INTEGER(KIND=sint), PARAMETER :: n_cwp_bins=14
    INTEGER(KIND=sint), PARAMETER :: n_cer_bins=11 

END MODULE COMMON_CONSTANTS
