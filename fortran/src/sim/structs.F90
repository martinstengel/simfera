!-------------------------------------------------------------------------------
! Name: structs.F90
!-------------------------------------------------------------------------------
MODULE STRUCTS 

    USE COMMON_CONSTANTS

    IMPLICIT NONE

    TYPE config
        REAL(KIND=sreal)           :: thv
        INTEGER(KIND=sint)         :: mpc, scops, overlap, cwc_mod
        INTEGER(KIND=sint)         :: sy, ey, sm, em, sd, ed
        CHARACTER(LEN=8)           :: start_date, end_date
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
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE :: cfc_high, cfc_mid, cfc_low
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE :: cph, cot, cwp, cer
        ! in-cloud model lwp and iwp
        REAL(KIND=sreal), DIMENSION(:), ALLOCATABLE :: mlwp, miwp
    END TYPE pseudo_arrays

    ! era-i input
    TYPE era_input
        INTEGER(KIND=lint)                              :: xdim, ydim, zdim
        REAL(KIND=sreal),   DIMENSION(:),   ALLOCATABLE :: lat, lon
        INTEGER(KIND=sint), DIMENSION(:),   ALLOCATABLE :: lev
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: cc_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lwc_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: iwc_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: temp_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: shum_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: geop_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: pres_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: dpres_prof
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: sza2d, lnsp2d, geop2d
        INTEGER(KIND=sint)         :: year, month, day, hour, doy
        CHARACTER(LEN=file_length) :: filename, dirname, basename
    END TYPE era_input


    ! temp arrays for each time slot
    TYPE tmp_arrays
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lwc_prof_inc
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: iwc_prof_inc
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lwp_prof, iwp_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lcer_prof, icer_prof
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: lcot_prof, icot_prof
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cfc, cph, cph_day
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cfc_high, cfc_mid, cfc_low
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: ctt, cth, ctp
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cwp, lwp, iwp
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: lwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: iwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cot, cot_liq, cot_ice
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: cer, cer_liq, cer_ice
        REAL(KIND=sreal), DIMENSION(:,:),   ALLOCATABLE :: mlwp, miwp
    END TYPE tmp_arrays

    
    ! final output (monthly mean - L3C)
    TYPE l3_vars
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cfc, cph, cph_day
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cfc_high, cfc_mid, cfc_low
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: ctt, cth, ctp
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cwp, lwp, iwp
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: lwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: iwp_allsky
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cot, cot_liq, cot_ice
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: cer, cer_liq, cer_ice
        REAL(KIND=sreal), DIMENSION(:,:), ALLOCATABLE :: mlwp, miwp
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
        INTEGER(KIND=lint), DIMENSION(:,:), ALLOCATABLE :: mlwp, miwp
    END TYPE npoints

    TYPE hist1d_ids
        INTEGER(KIND=lint) :: cot_axis, cot_bins, cot_hist
        INTEGER(KIND=lint) :: ctp_axis, ctp_bins, ctp_hist
        INTEGER(KIND=lint) :: ctt_axis, ctt_bins, ctt_hist
        INTEGER(KIND=lint) :: cer_axis, cer_bins, cer_hist
        INTEGER(KIND=lint) :: cwp_axis, cwp_bins, cwp_hist
    END TYPE hist1d_ids

    TYPE hist2d_ids
        INTEGER(KIND=lint) :: cot_axis, cot_bins
        INTEGER(KIND=lint) :: ctp_axis, ctp_bins
        INTEGER(KIND=lint) :: hist
    END TYPE hist2d_ids

    TYPE mm_ids
        INTEGER(KIND=lint) :: cfc, cph, cph_day
        INTEGER(KIND=lint) :: cfc_high, cfc_mid, cfc_low
        INTEGER(KIND=lint) :: ctp, cth, ctt
        INTEGER(KIND=lint) :: cot, cot_liq, cot_ice
        INTEGER(KIND=lint) :: cer, cer_liq, cer_ice
        INTEGER(KIND=lint) :: cwp, lwp, iwp
        INTEGER(KIND=lint) :: mlwp, miwp
        INTEGER(KIND=lint) :: cwp_allsky, lwp_allsky, iwp_allsky
        INTEGER(KIND=lint) :: nobs, nobs_lwp, nobs_iwp
        INTEGER(KIND=lint) :: nobs_mlwp, nobs_miwp
    END TYPE mm_ids

END MODULE STRUCTS
