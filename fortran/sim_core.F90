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
        USE UNDEFINE
        USE STRUCTS

        IMPLICIT NONE

        TYPE(config),     INTENT(IN)    :: set
        TYPE(era_input),  INTENT(IN)    :: inp
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp
        TYPE(l3_vars),    INTENT(INOUT) :: fin

        ! local variables
        INTEGER(KIND=sint) :: xi, yi, flag, nlev, ncol
        TYPE(scops_matrix) :: matrix

        !sreal_fill_value=-999.0

        PRINT*, "** MAIN_PROC: includes "
        PRINT*, "   Downscaling (scops)"
        PRINT*, "   Pseudo-Retrieval"
        PRINT*, "   Computing summary statistics"

        CALL ALLOCATE_TMP_VARS( inp, tmp )

        ncol = 20           !number of subcolumns (profiles)
        nlev = inp % zdim-1 !number of layers, i.e. model levels

        DO xi=1, inp % xdim     !longitude
            DO yi=1, inp % ydim !latitude

                IF ( inp % sza2d(xi,yi) .GE. 75.0 ) THEN
                    flag = is_night
                ELSE
                    flag = is_day
                END IF

                CALL ALLOCATE_MATRIX( flag, ncol, nlev, matrix )
                CALL DOWNSCALING( set % scops, set % mpc, flag, &
                                  nlev, ncol, inp % cc(xi,yi,:),&
                                  tmp % lcot_lay(xi,yi,:),      &
                                  tmp % icot_lay(xi,yi,:),      &
                                  tmp % lcer_lay(xi,yi,:),      &
                                  tmp % icer_lay(xi,yi,:),      &
                                  tmp % lwp_lay(xi,yi,:),       &
                                  tmp % iwp_lay(xi,yi,:),       &
                                  matrix )
                stop

                CALL UNDEFINE_MATRIX( matrix )

            END DO !end of yi=latitude
        END DO     !end of xi=longitude

    END SUBROUTINE MAIN_PROC

    !==========================================================================

    SUBROUTINE ALLOCATE_MATRIX( flag, ncol, nlev, matrix )
    
        USE COMMON_CONSTANTS
        USE STRUCTS
        IMPLICIT NONE
        INTEGER(KIND=sint), INTENT(IN)    :: ncol, nlev, flag
        TYPE(scops_matrix), INTENT(INOUT) :: matrix

        ALLOCATE( matrix % tcot ( ncol, nlev ) )
        ALLOCATE( matrix % cot ( ncol, nlev ) )
        ALLOCATE( matrix % cfc ( ncol, nlev ) )
        ALLOCATE( matrix % cph ( ncol, nlev ) )

        IF ( flag == is_day ) THEN 
            ALLOCATE( matrix % cwp ( ncol, nlev ) )
            ALLOCATE( matrix % cer ( ncol, nlev ) )
        END IF

    END SUBROUTINE ALLOCATE_MATRIX
    
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

    SUBROUTINE DOWNSCALING( overlap, mpc, flag, nlev, ncol, icc, &
                            lcot, icot, lcer, icer, lwp, iwp, matrix )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        INTEGER(KIND=sint),                   INTENT(IN) :: overlap, mpc
        INTEGER(KIND=sint),                   INTENT(IN) :: flag, nlev, ncol
        REAL(KIND=sreal), DIMENSION(nlev),    INTENT(IN) :: lcot, icot
        REAL(KIND=sreal), DIMENSION(nlev),    INTENT(IN) :: lcer, icer
        REAL(KIND=sreal), DIMENSION(nlev),    INTENT(IN) :: lwp, iwp
        REAL(KIND=sreal), DIMENSION(nlev+1),  INTENT(IN) :: icc
        TYPE(scops_matrix),                INTENT(INOUT) :: matrix

        ! local variables
        INTEGER(KIND=sint)                  :: zi, lastcloud
        INTEGER(KIND=sint)                  :: nfb, nfb_liq, nfb_ice
        INTEGER(KIND=sint), DIMENSION(ncol) :: ci
        REAL(KIND=sreal)                    :: cwp_all, cot_all
        REAL(KIND=sreal),   DIMENSION(nlev) :: cwp_profile
        REAL(KIND=sreal),   DIMENSION(nlev) :: cot_profile
        REAL(KIND=sreal),   DIMENSION(nlev) :: cfc_profile


        matrix % tcot =  0.0
        matrix % cot  =  0.0
        matrix % cfc  =  0.0
        matrix % cph  = -1.0

        IF ( flag == is_day ) THEN
            matrix % cwp =  0.0
            matrix % cer = -1.0
        END IF


        cwp_profile = lwp + iwp
        cot_profile = lcot + icot
        cfc_profile = icc(1:nlev+1-1)*0.5 + icc(2:nlev+1)*0.5 !nlay=nlev+1


        lastcloud = -2

        DO, zi=1, nlev 

            nfb = FLOOR( ncol * cfc_profile(zi) )

            IF ( nfb > 0 ) THEN

                nfb_liq = -1
                nfb_ice = -1
                cwp_all = lwp(zi) + iwp(zi)
                cot_all = lcot(zi) + icot(zi)

                ! only needed for mpc == no_mixed_phase
                IF ( cwp_all > 0.0 ) THEN 
                    ! number of liquid clouds
                    nfb_liq = FLOOR( nfb * (lwp(zi)/cwp_all) )
                    ! number of ice clouds
                    nfb_ice = nfb - nfb_liq
                END IF

                IF ( lastcloud .NE. (zi-1) .OR. overlap == rand) THEN
                    CALL GET_RANDOMU( ncol, ci )
                    print*, ncol
                    print*, ci
                END IF

                stop
            END IF !end of nfb > 0 if-loop

        END DO !end of zi-loop

    END SUBROUTINE DOWNSCALING

    !==========================================================================

    SUBROUTINE GET_RANDOMU( ncol, indices )

        USE COMMON_CONSTANTS
        USE FUNCS, only: RANDOM_NUMBER_DUPLICATES

        IMPLICIT NONE

        INTEGER(KIND=sint),                  INTENT(IN)    :: ncol
        INTEGER(KIND=sint), DIMENSION(ncol), INTENT(INOUT) :: indices

        ! local variables
        INTEGER(KIND=sint)                            :: i, seedSize
        INTEGER(KIND=sint), DIMENSION(:), ALLOCATABLE :: seed
        INTEGER(KIND=sint), DIMENSION(ncol)           :: xlindgen
        REAL(KIND=sreal),    DIMENSION(ncol)          :: rndRealArr
        INTEGER, DIMENSION(8) :: dtVals
        LOGICAL               :: is_duplicate

        ! Create integer vector with ncol elements
        DO i = 1, ncol
            xlindgen(i) = i
        END DO 
        
        ! Get unique clock values
        CALL DATE_AND_TIME(VALUES=dtVals)
        ! The seed is generally 4 or 8 integers long. We find out now.
        CALL RANDOM_SEED( SIZE=seedSize )
        ! We intend to use the DATE_AND_TIME value to initialize the seed 
        ! if it's bit enough
        IF ( seedSize > 8 ) THEN
            PRINT*, " -- ERROR: Seed size too large to init with DATE_AND_TIME "
            STOP
        END IF

        ! Allocate seed with known size
        ALLOCATE( seed(seedSize) )
        ! Get old seed
        CALL RANDOM_SEED( GET=seed )
        ! Use the last bits of the DATE_AND_TIME values array to seed the number
        CALL RANDOM_SEED( PUT=dtVals((9-seedSize):8) )
        ! Re-query the seed to make sure it worked.
        CALL RANDOM_SEED( GET=seed )

        ! Get an array of random numbers using RANDOM_NUMBER, which
        ! Returns a single pseudorandom number or an array of pseudorandom
        ! numbers from the uniform distribution over the range 0 < x < 1.
        rndRealArr = 0.0
        CALL RANDOM_NUMBER( rndRealArr )

        ! Check for duplicates in rndRealArr
        is_duplicate = RANDOM_NUMBER_DUPLICATES( ncol, rndRealArr )
        IF ( is_duplicate ) THEN
            PRINT*, " -- ERROR: dupliate random number!"
            STOP
        END IF

        ! Sort rndRealArr in ascending order and apply it to indices
        indices = xlindgen
        CALL GET_INDICES( ncol, rndRealArr, indices )

    END SUBROUTINE GET_RANDOMU

    !==========================================================================

    SUBROUTINE GET_INDICES ( n, arr, brr )

        USE COMMON_CONSTANTS
        IMPLICIT NONE

        INTEGER(KIND=sint)                              :: n   !ncols
        REAL(KIND=sreal),   DIMENSION(n), INTENT(INOUT) :: arr !real values
        INTEGER(KIND=sint), DIMENSION(n), INTENT(INOUT) :: brr !integer values
        ! local variables
        INTEGER :: i, j
        REAL    :: a, b

        outer: DO j=2, n

            a = arr(j)
            b = brr(j)

            inner: DO i = j-1, 1, -1
                IF ( arr(i) .LE. a ) EXIT inner
                arr(i+1) = arr(i)
                brr(i+1) = brr(i)
            END DO inner

            arr(i+1) = a
            brr(i+1) = b

            i = 0

        END DO outer

    END SUBROUTINE GET_INDICES

    !==========================================================================

END MODULE SIM_CORE
