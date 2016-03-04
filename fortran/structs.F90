!-------------------------------------------------------------------------------
! Name: structs.F90
!-------------------------------------------------------------------------------
MODULE STRUCTS 

    USE COMMON_CONSTANTS

    IMPLICIT NONE

    TYPE config
        REAL(KIND=sreal)   :: thv
        INTEGER(KIND=sint) :: mpc
        INTEGER(KIND=sint) :: scops 
        INTEGER(KIND=sint) :: sy
        INTEGER(KIND=sint) :: ey
        INTEGER(KIND=sint) :: sm
        INTEGER(KIND=sint) :: em
        INTEGER(KIND=sint) :: sd
        INTEGER(KIND=sint) :: ed
        CHARACTER(LEN=8) :: start_date
        CHARACTER(LEN=8) :: end_date
        CHARACTER(LEN=path_length) :: rep_path
        CHARACTER(LEN=path_length) :: inp_path
        CHARACTER(LEN=path_length) :: out_path
        CHARACTER(LEN=path_length) :: sst_file
        ! 2d histogram definitions
        REAL(KIND=sreal) :: hist_cot(n_hist_cot)
        REAL(KIND=sreal) :: hist_ctp(n_hist_ctp)
        REAL(KIND=sreal) :: hist_cot_bin(n_hist_cot-1)
        REAL(KIND=sreal) :: hist_ctp_bin(n_hist_ctp-1)
        ! 1d histogram definitions
        REAL(KIND=sreal) :: hist_cot_1d_bin(n_cot_bins)
        REAL(KIND=sreal) :: hist_cot_1d_axis(n_cot_bins+1)
        REAL(KIND=sreal) :: hist_ctp_1d_bin(n_ctp_bins)
        REAL(KIND=sreal) :: hist_ctp_1d_axis(n_ctp_bins+1)
        REAL(KIND=sreal) :: hist_ctt_1d_bin(n_ctt_bins)
        REAL(KIND=sreal) :: hist_ctt_1d_axis(n_ctt_bins+1)
        REAL(KIND=sreal) :: hist_cer_1d_bin(n_cer_bins)
        REAL(KIND=sreal) :: hist_cer_1d_axis(n_cer_bins+1)
        REAL(KIND=sreal) :: hist_cwp_1d_bin(n_cwp_bins)
        REAL(KIND=sreal) :: hist_cwp_1d_axis(n_cwp_bins+1)
    END TYPE config

    ! era-sst
    TYPE era_sst_lsm
        INTEGER(KIND=lint)                              :: nlon, nlat
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE     :: lat, lon
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE   :: sst
        INTEGER(KIND=sint), DIMENSION(:,:), ALLOCATABLE :: lsm
    END TYPE era_sst_lsm

    ! era-i input
    TYPE era_input
        INTEGER(KIND=lint)                              :: nlon, nlat, nlev
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE     :: lat, lon
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE     :: plevel, dpres
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: cc, lwc, iwc
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: geop, temp
        INTEGER(KIND=sint)         :: year, month, day, hour
        CHARACTER(LEN=file_length) :: filename, dirname, basename
    END TYPE era_input


    ! counts
    TYPE l3_points
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: raw
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cfc
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: ctp
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cwp
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cwp_allsky
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: lwp
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: lwp_allsky
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: iwp
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: iwp_allsky
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cot
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cot_liq
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cot_ice
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cer
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cer_liq
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cer_ice
        INTEGER(KIND=lint), DIMENSION(:,:), POINTER :: cph_day
    END TYPE l3_points


    TYPE l3_vars
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cfc
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cph
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cph_day
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: ctt
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cth
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: ctp
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cwp
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: lwp
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: lwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: iwp
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: iwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cot
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cot_liq
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cot_ice
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cer
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cer_liq
        REAL(KIND=sreal), DIMENSION(:,:), POINTER :: cer_ice
        INTEGER(KIND=lint), DIMENSION(:,:,:,:,:), POINTER :: hist_cot_ctp
        INTEGER(KIND=lint), DIMENSION(:,:,:,:), POINTER   :: hist_ctp
        INTEGER(KIND=lint), DIMENSION(:,:,:,:), POINTER   :: hist_ctt
        INTEGER(KIND=lint), DIMENSION(:,:,:,:), POINTER   :: hist_cot
        INTEGER(KIND=lint), DIMENSION(:,:,:,:), POINTER   :: hist_cwp
        INTEGER(KIND=lint), DIMENSION(:,:,:,:), POINTER   :: hist_cer
    END TYPE l3_vars


END MODULE STRUCTS
