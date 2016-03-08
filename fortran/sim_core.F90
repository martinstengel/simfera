!-------------------------------------------------------------------------------
! Name: sim_core.F90
!-------------------------------------------------------------------------------
MODULE SIM_CORE 

    CONTAINS

    !==========================================================================
    ! The main part of the cloud simulator
    !==========================================================================

    SUBROUTINE MAIN_PROC( set, inp, tmp, fin )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        INTEGER(KIND=sint)              :: xi, yi, flag, nlev
        TYPE(config),     INTENT(IN)    :: set
        TYPE(era_input),  INTENT(IN)    :: inp
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp
        TYPE(l3_vars),    INTENT(INOUT) :: fin
        TYPE(scops_matrix)              :: matrix

        !sreal_fill_value=-999.0

        PRINT*, "** MAIN_PROC"

        CALL ALLOCATE_TMP_VARS( inp, tmp )

        nlev = inp % zdim-1 !number of layers, i.e. model levels

        DO xi=1, inp % xdim     !longitude
            DO yi=1, inp % ydim !latitude

                IF ( inp % sza2d(xi,yi) .GE. 75.0 ) THEN
                    flag = is_night
                ELSE
                    flag = is_day
                END IF

                PRINT*, "   Downscaling (scops)"
                CALL DOWNSCALING( set % scops, set % mpc, flag, &
                                  nlev, inp % cc(xi,yi,:),      &
                                  tmp % lcot_lay(xi,yi,:),      &
                                  tmp % icot_lay(xi,yi,:),      &
                                  tmp % lcer_lay(xi,yi,:),      &
                                  tmp % icer_lay(xi,yi,:),      &
                                  tmp % lwp_lay(xi,yi,:),       &
                                  tmp % iwp_lay(xi,yi,:),       &
                                  matrix )
                stop

            END DO !end of yi=latitude
        END DO     !end of xi=longitude


        PRINT*, "   Pseudo-Retrieval"

        PRINT*, "   Compute summary statistics"

    END SUBROUTINE MAIN_PROC

    !==========================================================================
    
    SUBROUTINE ALLOCATE_TMP_VARS( inp, tmp )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(era_input),  INTENT(IN)    :: inp
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp

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

    END SUBROUTINE ALLOCATE_TMP_VARS

    !==========================================================================

    SUBROUTINE DOWNSCALING( scops_type, mpc, flag, nlev, icc, &
                            lcot, icot, lcer, icer, lwp, iwp, subcols )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        INTEGER(KIND=sint),                  INTENT(IN)  :: scops_type, mpc
        INTEGER(KIND=sint),                  INTENT(IN)  :: flag, nlev
        REAL(KIND=sreal), DIMENSION(nlev),   INTENT(IN)  :: lcot, icot
        REAL(KIND=sreal), DIMENSION(nlev),   INTENT(IN)  :: lcer, icer
        REAL(KIND=sreal), DIMENSION(nlev),   INTENT(IN)  :: lwp, iwp
        REAL(KIND=sreal), DIMENSION(nlev+1), INTENT(IN)  :: icc
        TYPE(scops_matrix),                  INTENT(OUT) :: subcols

        print('(27F14.6)'), icc

    END SUBROUTINE DOWNSCALING

    !==========================================================================

END MODULE SIM_CORE
