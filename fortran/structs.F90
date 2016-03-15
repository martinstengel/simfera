!-------------------------------------------------------------------------------
! Name: structs.F90
!-------------------------------------------------------------------------------
MODULE STRUCTS 

    USE COMMON_CONSTANTS

    IMPLICIT NONE

    TYPE config
        REAL(KIND=sreal)           :: thv
        INTEGER(KIND=sint)         :: mpc, scops
        INTEGER(KIND=sint)         :: sy, ey, sm, em, sd, ed
        CHARACTER(LEN=8)           :: start_date, end_date
        CHARACTER(LEN=path_length) :: rep_path
        CHARACTER(LEN=path_length) :: inp_path
        CHARACTER(LEN=path_length) :: out_path
        CHARACTER(LEN=path_length) :: sst_file
        ! 2d histogram definitions
        REAL(KIND=sreal) :: hist_phase(n_hist_phase)
        REAL(KIND=sreal) :: hist_cot_2d_axis(n_hist_cot)
        REAL(KIND=sreal) :: hist_cot_2d_bin(n_hist_cot-1)
        REAL(KIND=sreal) :: hist_ctp_2d_axis(n_hist_ctp)
        REAL(KIND=sreal) :: hist_ctp_2d_bin(n_hist_ctp-1)
        ! 1d histogram definitions
        REAL(KIND=sreal) :: hist_cot_1d_axis(n_cot_bins+1)
        REAL(KIND=sreal) :: hist_cot_1d_bin(n_cot_bins)
        REAL(KIND=sreal) :: hist_ctp_1d_axis(n_ctp_bins+1)
        REAL(KIND=sreal) :: hist_ctp_1d_bin(n_ctp_bins)
        REAL(KIND=sreal) :: hist_ctt_1d_axis(n_ctt_bins+1)
        REAL(KIND=sreal) :: hist_ctt_1d_bin(n_ctt_bins)
        REAL(KIND=sreal) :: hist_cer_1d_axis(n_cer_bins+1)
        REAL(KIND=sreal) :: hist_cer_1d_bin(n_cer_bins)
        REAL(KIND=sreal) :: hist_cwp_1d_axis(n_cwp_bins+1)
        REAL(KIND=sreal) :: hist_cwp_1d_bin(n_cwp_bins)
    END TYPE config

    ! era-sst
    TYPE era_aux
        INTEGER(KIND=lint)                              :: nlon, nlat
        REAL(KIND=sreal), DIMENSION(:),     ALLOCATABLE :: lat, lon
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: lat2d, lon2d
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: sst2d
        INTEGER(KIND=sint), DIMENSION(:,:), ALLOCATABLE :: lsm2d
    END TYPE era_aux

    ! gridbox mean profile downscaling to gridbox subcolumn profiles
    TYPE scops_matrix
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: tcot, cot
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cfc, cph, cwp, cer
    END TYPE scops_matrix

    ! pseudo arrays(ncol)
    TYPE pseudo_arrays
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE :: ctp, cth, ctt, cfc
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE :: cph, cot, cwp, cer
    END TYPE pseudo_arrays

    ! era-i input
    TYPE era_input
        INTEGER(KIND=lint)                              :: xdim, ydim, zdim
        REAL(KIND=sreal), DIMENSION(:),     ALLOCATABLE :: lat, lon
        REAL(KIND=sreal), DIMENSION(:),     ALLOCATABLE :: plevel, dpres
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: cc, lwc, iwc
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: geop, temp
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: sza2d
        INTEGER(KIND=sint)         :: year, month, day, hour, doy
        CHARACTER(LEN=file_length) :: filename, dirname, basename
    END TYPE era_input


    ! temp arrays for each time slot
    TYPE tmp_arrays
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lwc_inc, iwc_inc
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lwp_lay, iwp_lay
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lcer_lay, icer_lay
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lcot_lay, icot_lay
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cfc, cph, cph_day
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: ctt, cth, ctp
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cwp, lwp, iwp
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: lwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: iwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cot, cot_liq, cot_ice
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cer, cer_liq, cer_ice
    END TYPE tmp_arrays

    
    ! final output (monthly mean - L3C)
    TYPE l3_vars
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cfc, cph, cph_day
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: ctt, cth, ctp
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cwp, lwp, iwp
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: lwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: iwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cot, cot_liq, cot_ice
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cer, cer_liq, cer_ice
        INTEGER(KIND=lint), DIMENSION(:,:,:,:,:), ALLOCATABLE :: hist_cot_ctp
        INTEGER(KIND=lint), DIMENSION(:,:,:,:),   ALLOCATABLE :: hist_ctp
        INTEGER(KIND=lint), DIMENSION(:,:,:,:),   ALLOCATABLE :: hist_ctt
        INTEGER(KIND=lint), DIMENSION(:,:,:,:),   ALLOCATABLE :: hist_cot
        INTEGER(KIND=lint), DIMENSION(:,:,:,:),   ALLOCATABLE :: hist_cwp
        INTEGER(KIND=lint), DIMENSION(:,:,:,:),   ALLOCATABLE :: hist_cer
    END TYPE l3_vars


    TYPE npoints
        INTEGER(KIND=sint) :: file_counter
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: cph_day
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: cfc, ctp
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: cwp, lwp, iwp
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: cwp_allsky
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: lwp_allsky
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: iwp_allsky
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: cot, cer
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: cot_liq, cer_liq
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: cot_ice, cer_ice
    END TYPE npoints


END MODULE STRUCTS
