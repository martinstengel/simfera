!-------------------------------------------------------------------------------
! Name: initialize.F90
!-------------------------------------------------------------------------------
MODULE INITIALIZE 

    CONTAINS

    !==========================================================================
    ! ALLOCATE ARRAYS
    !==========================================================================

    SUBROUTINE INITIALIZE_COUNTS( aux, cnt )

        USE STRUCTS

        IMPLICIT NONE

        TYPE(era_aux), INTENT(IN)    :: aux
        TYPE(npoints), INTENT(INOUT) :: cnt

        ALLOCATE( cnt % cfc ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % ctp ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cwp ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % lwp ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % iwp ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cwp_allsky ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % lwp_allsky ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % iwp_allsky ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cot ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cot_liq ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cot_ice ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cer ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cer_liq ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cer_ice ( aux % nlon, aux % nlat) )
        ALLOCATE( cnt % cph_day ( aux % nlon, aux % nlat) )

        cnt % file_counter = 0
        cnt % cfc = 0
        cnt % ctp = 0
        cnt % cwp = 0
        cnt % lwp = 0
        cnt % iwp = 0
        cnt % cwp_allsky = 0
        cnt % lwp_allsky = 0
        cnt % iwp_allsky = 0
        cnt % cot = 0
        cnt % cot_liq = 0
        cnt % cot_ice = 0
        cnt % cer = 0
        cnt % cer_liq = 0
        cnt % cer_ice = 0
        cnt % cph_day = 0

    END SUBROUTINE INITIALIZE_COUNTS

    !==========================================================================

    SUBROUTINE INITIALIZE_FINAL( cfg, aux, fin )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        TYPE(config),     INTENT(IN) :: cfg
        TYPE(era_aux),    INTENT(IN) :: aux
        TYPE(l3_vars), INTENT(INOUT) :: fin

        ALLOCATE( fin % cfc ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cph ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cph_day ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % ctp ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cth ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % ctt ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cwp ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % lwp ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % iwp ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cwp_allsky ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % lwp_allsky ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % iwp_allsky ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cot ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cot_liq ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cot_ice ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cer ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cer_liq ( aux % nlon, aux % nlat) )
        ALLOCATE( fin % cer_ice ( aux % nlon, aux % nlat) )


        ALLOCATE( fin % hist_cot_ctp ( aux % nlon, aux % nlat, &
            SIZE(cfg % hist_cot_2d_bin), SIZE(cfg % hist_ctp_2d_bin), n_hist_phase ) )

        ALLOCATE( fin % hist_ctp ( aux % nlon, aux % nlat, &
            SIZE(cfg % hist_ctp_1d_bin), n_hist_phase) )

        ALLOCATE( fin % hist_ctt ( aux % nlon, aux % nlat, &
            SIZE(cfg % hist_ctt_1d_bin), n_hist_phase ) )

        ALLOCATE( fin % hist_cot ( aux % nlon, aux % nlat, &
            SIZE(cfg % hist_cot_1d_bin), n_hist_phase ) )

        ALLOCATE( fin % hist_cwp ( aux % nlon, aux % nlat, &
            SIZE(cfg % hist_cwp_1d_bin), n_hist_phase ) )

        ALLOCATE( fin % hist_cer ( aux % nlon, aux % nlat, &
            SIZE(cfg % hist_cer_1d_bin), n_hist_phase ) )

        fin % cfc = 0.0
        fin % cph = 0.0   
        fin % cph_day = 0.0   
        fin % ctp = 0.0
        fin % cth = 0.0   
        fin % ctt = 0.0   
        fin % cwp = 0.0   
        fin % lwp = 0.0   
        fin % iwp = 0.0   
        fin % cwp_allsky = 0.0 
        fin % lwp_allsky = 0.0
        fin % iwp_allsky = 0.0
        fin % cot = 0.0
        fin % cot_liq = 0.0   
        fin % cot_ice = 0.0
        fin % cer = 0.0
        fin % cer_liq = 0.0
        fin % cer_ice = 0.0

        fin % hist_cot_ctp = 0
        fin % hist_ctp = 0
        fin % hist_ctt = 0
        fin % hist_cot = 0
        fin % hist_cwp = 0
        fin % hist_cer = 0

    END SUBROUTINE INITIALIZE_FINAL

    !==========================================================================

    SUBROUTINE INITIALIZE_TEMPS( inp, tmp )

        USE STRUCTS

        IMPLICIT NONE

        TYPE(era_input),  INTENT(IN)    :: inp
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp

        ALLOCATE( tmp % lwc_inc (inp % xdim, inp % ydim, inp % zdim) )
        ALLOCATE( tmp % iwc_inc (inp % xdim, inp % ydim, inp % zdim) )
        ALLOCATE( tmp % lwp_lay (inp % xdim, inp % ydim, inp % zdim) )
        ALLOCATE( tmp % iwp_lay (inp % xdim, inp % ydim, inp % zdim) )
        ALLOCATE( tmp % lcot_lay (inp % xdim, inp % ydim, inp % zdim) )
        ALLOCATE( tmp % icot_lay (inp % xdim, inp % ydim, inp % zdim) )
        ALLOCATE( tmp % lcer_lay (inp % xdim, inp % ydim, inp % zdim) )
        ALLOCATE( tmp % icer_lay (inp % xdim, inp % ydim, inp % zdim) )

        ALLOCATE( tmp % cfc (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % ctp (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cth (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % ctt (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cph (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cph_day (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cwp (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % lwp (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % iwp (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cwp_allsky (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % lwp_allsky (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % iwp_allsky (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cot (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cot_liq (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cot_ice (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cer (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cer_liq (inp % xdim, inp % ydim) )
        ALLOCATE( tmp % cer_ice (inp % xdim, inp % ydim) )

        tmp % lwc_inc  = 0.0 
        tmp % iwc_inc  = 0.0
        tmp % lwp_lay  = 0.0
        tmp % iwp_lay  = 0.0
        tmp % lcot_lay = 0.0
        tmp % icot_lay = 0.0
        tmp % lcer_lay = 0.0
        tmp % icer_lay = 0.0

        tmp % cfc = 0.0
        tmp % ctp = 0.0
        tmp % cth = 0.0
        tmp % ctt = 0.0
        tmp % cph = 0.0
        tmp % cph_day = 0.0
        tmp % cwp = 0.0
        tmp % lwp = 0.0
        tmp % iwp = 0.0
        tmp % cwp_allsky = 0.0
        tmp % lwp_allsky = 0.0
        tmp % iwp_allsky = 0.0
        tmp % cot = 0.0
        tmp % cot_liq = 0.0
        tmp % cot_ice = 0.0
        tmp % cer = 0.0
        tmp % cer_liq = 0.0
        tmp % cer_ice = 0.0

    END SUBROUTINE INITIALIZE_TEMPS

    !==========================================================================

    SUBROUTINE INITIALIZE_MATRIX( flag, ncol, nlev, matrix )
    
        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        INTEGER(KIND=sint), INTENT(IN)    :: ncol, nlev, flag
        TYPE(scops_matrix), INTENT(INOUT) :: matrix

        ALLOCATE( matrix % tcot ( ncol, nlev ) )
        ALLOCATE( matrix % cot ( ncol, nlev ) )
        ALLOCATE( matrix % cfc ( ncol, nlev ) )
        ALLOCATE( matrix % cph ( ncol, nlev ) )

        matrix % tcot =  0.0
        matrix % cot  =  0.0
        matrix % cfc  =  0.0
        matrix % cph  = -1.0

        IF ( flag == is_day ) THEN 

            ALLOCATE( matrix % cwp ( ncol, nlev ) )
            ALLOCATE( matrix % cer ( ncol, nlev ) )

            matrix % cwp =  0.0
            matrix % cer = -1.0

        END IF

    END SUBROUTINE INITIALIZE_MATRIX
    
    !==========================================================================

    SUBROUTINE INITIALIZE_ARRAYS( flag, ncol, array )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        INTEGER(KIND=sint),  INTENT(IN)    :: flag, ncol
        TYPE(pseudo_arrays), INTENT(INOUT) :: array

        ALLOCATE( array % ctp ( ncol ) )
        ALLOCATE( array % cth ( ncol ) )
        ALLOCATE( array % ctt ( ncol ) )
        ALLOCATE( array % cfc ( ncol ) )
        ALLOCATE( array % cph ( ncol ) )

        array % ctp = -1.0
        array % cth = -1.0
        array % ctt = -1.0
        array % cfc =  0.0
        array % cph = -1.0

        IF ( flag == is_day ) THEN 

            ALLOCATE( array % cot ( ncol ) )
            ALLOCATE( array % cwp ( ncol ) )
            ALLOCATE( array % cer ( ncol ) )

            array % cot = -1.0 
            array % cwp = -1.0 
            array % cer = -1.0 
        END IF

    END SUBROUTINE INITIALIZE_ARRAYS

    !==========================================================================

END MODULE INITIALIZE
