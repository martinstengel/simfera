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
        USE INITIALIZE
        USE UNDEFINE
        USE STRUCTS
        USE mod_rng, ONLY: rng_state, init_rng

        IMPLICIT NONE

        TYPE(config),     INTENT(IN)    :: set
        TYPE(era_input),  INTENT(IN)    :: inp
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp
        TYPE(l3_vars),    INTENT(INOUT) :: fin

        ! local variables
        INTEGER(KIND=sint)  :: nlev, ncol
        INTEGER(KIND=sint)  :: xi, yi, flag
        TYPE(scops_matrix)  :: matrix
        TYPE(pseudo_arrays) :: array
        INTEGER(KIND=sint)  :: npoints_model
        ! random number generator
        INTEGER, DIMENSION(:), ALLOCATABLE         :: seed
        TYPE(rng_state), ALLOCATABLE, DIMENSION(:) :: rngs


        PRINT*, "** MAIN_PROC: includes "
        PRINT*, "   Downscaling (scops)"
        PRINT*, "   Pseudo-Retrieval"
        PRINT*, "   Computing summary statistics"


        npoints_model = 1   !number of model points in the horizontal
        ncol = 20           !number of subcolumns (profiles)
        nlev = inp % zdim-1 !number of layers, i.e. model levels

        DO xi=1, inp % xdim     !longitude
            DO yi=1, inp % ydim !latitude

                IF ( inp % sza2d(xi,yi) .GE. 75.0 ) THEN
                    flag = is_night
                ELSE
                    flag = is_day
                END IF

                CALL INITIALIZE_MATRIX( flag, ncol, nlev, matrix )
                CALL INITIALIZE_ARRAYS( flag, ncol, array )


                ! ------------------------
                ! Random number generator
                ! ------------------------

                CALL GET_SEED( inp % temp(xi,yi,:), &
                               nlev, npoints_model, seed, rngs )
                CALL INIT_RNG( rngs, seed )


                ! ------------------------------------------------------------
                ! Generate subcolumns for clouds: return "matrix" structure
                ! ------------------------------------------------------------

                IF ( set % scops == cosp_scops ) THEN 

                    CALL COSP_SUBCOLS( ncol, nlev, flag,         &
                                       npoints_model,            &
                                       seed, rngs,               &
                                       set % overlap,            &
                                       inp % cc(xi,yi,:),        &
                                       tmp % lcot_lay(xi,yi,:),  &
                                       tmp % icot_lay(xi,yi,:),  &
                                       tmp % lcer_lay(xi,yi,:),  &
                                       tmp % icer_lay(xi,yi,:),  &
                                       tmp % lwp_lay(xi,yi,:),   &
                                       tmp % iwp_lay(xi,yi,:),   &
                                       matrix )

                ELSEIF ( set % scops == dwd_scops ) THEN 

                    CALL DWD_SUBCOLS( ncol, nlev, flag,         &
                                      set % overlap,            &
                                      set % mpc,                &
                                      inp % cc(xi,yi,:),        &
                                      tmp % lcot_lay(xi,yi,:),  &
                                      tmp % icot_lay(xi,yi,:),  &
                                      tmp % lcer_lay(xi,yi,:),  &
                                      tmp % icer_lay(xi,yi,:),  &
                                      tmp % lwp_lay(xi,yi,:),   &
                                      tmp % iwp_lay(xi,yi,:),   &
                                      matrix )
                ELSE 
                    PRINT*, "This scops type is not defined: 1=dwd, 2=cosp"
                    stop
                END IF

                CALL PSEUDO_RETRIEVAL( set % thv, flag, ncol, nlev, &
                                       xi, yi, inp, matrix, array )


                CALL COMPUTE_SUMMARY_STATISTICS( xi, yi, ncol, flag, &
                                                 array, set, tmp, fin )



                CALL UNDEFINE_MATRIX( matrix )
                CALL UNDEFINE_ARRAYS( array )

                DEALLOCATE( rngs )
                DEALLOCATE( seed )

            END DO !end of yi=latitude
        END DO     !end of xi=longitude

    END SUBROUTINE MAIN_PROC

    !==========================================================================

    SUBROUTINE DWD_SUBCOLS( ncol, nlev, flag, overlap, mpc, &
                      icc, lcot, icot, lcer, icer, lwp, iwp, matrix )

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
        INTEGER(KIND=sint)                  :: von, bis
        INTEGER(KIND=sint), DIMENSION(ncol) :: ci
        REAL(KIND=sreal)                    :: cwp_all, cot_all
        REAL(KIND=sreal),   DIMENSION(nlev) :: cfc_profile


        lastcloud = -2

        cfc_profile = icc(1:nlev+1-1)*0.5 + icc(2:nlev+1)*0.5

        DO, zi=1, nlev !loop over model levels 

            ! number of filled boxes
            nfb = NINT( ncol * cfc_profile(zi) )

            IF ( nfb > ncol ) nfb = ncol

            IF ( nfb > 0 ) THEN

                nfb_liq = -1
                nfb_ice = -1
                cwp_all = lwp(zi)  + iwp(zi)
                cot_all = lcot(zi) + icot(zi)

                IF ( lastcloud .NE. (zi-1) .OR. overlap == over_rand ) &
                    CALL GET_RANDOMU( ncol, ci )

                IF ( mpc == no_mixed_phase ) THEN

                    IF ( cwp_all > 0.0 ) THEN 
                        nfb_liq = FLOOR( nfb * ( lwp(zi)/cwp_all ) )
                        nfb_ice = nfb - nfb_liq
                    END IF

                    IF ( nfb_ice > 0 ) THEN

                        von = 1
                        bis = nfb_ice

                        matrix % cot (ci(von:bis),zi) = icot(zi)
                        matrix % cfc (ci(von:bis),zi) = is_cloud
                        matrix % cph (ci(von:bis),zi) = is_ice

                        IF ( flag == is_day ) THEN 
                            matrix % cwp (ci(von:bis),zi) = iwp(zi)
                            matrix % cer (ci(von:bis),zi) = icer(zi)
                        END IF

                    END IF

                    IF ( nfb_liq > 0 ) THEN

                        von = nfb_ice+1
                        bis = nfb_ice+nfb_liq

                        matrix % cot( ci(von:bis), zi ) = lcot(zi)
                        matrix % cfc( ci(von:bis), zi ) = is_cloud
                        matrix % cph( ci(von:bis), zi ) = is_liq

                        IF ( flag == is_day ) THEN 
                            matrix % cwp (ci(von:bis),zi) = lwp(zi)
                            matrix % cer (ci(von:bis),zi) = lcer(zi)
                        END IF

                    END IF

                ELSE !mpc == mixed_phase

                    von = 1
                    bis = nfb

                    matrix % cot (ci(von:bis),zi) = cot_all
                    matrix % cfc (ci(von:bis),zi) = is_cloud

                    IF ( flag == is_day ) &
                        matrix % cwp (ci(von:bis),zi) = cwp_all

                    IF ( cwp_all > 0 ) THEN

                        ! liquid fraction
                        matrix % cph (ci(von:bis),zi) = lwp(zi) / cwp_all

                        !weighted mean CER day only
                        IF ( flag == is_day ) matrix % cer (ci(von:bis),zi) = & 
                            ( lcer(zi)*lwp(zi) + icer(zi)*iwp(zi) ) / cwp_all

                    END IF

                END IF !endif of mpc options

                lastcloud = zi

            END IF !end of nfb > 0 if-loop

            !TOP = upper-most layer: zi=1
            IF ( zi == 1 ) matrix % tcot(:,zi) = matrix % cot(:,1) 
            IF ( zi  > 1 ) matrix % tcot(:,zi) = SUM( matrix % cot(:,1:zi), 2 )

        END DO !end of zi-loop

    END SUBROUTINE DWD_SUBCOLS

    !==========================================================================

    SUBROUTINE GET_RANDOMU( ncol, indices )

        USE COMMON_CONSTANTS
        USE FUNCS, only: RANDOM_NUMBER_DUPLICATES

        IMPLICIT NONE

        INTEGER(KIND=sint),                  INTENT(IN)    :: ncol
        INTEGER(KIND=sint), DIMENSION(ncol), INTENT(INOUT) :: indices

        ! local variables
        INTEGER(KIND=sint)                  :: i
        INTEGER(KIND=sint), DIMENSION(ncol) :: xlindgen
        REAL(KIND=sreal),   DIMENSION(ncol) :: rndRealArr
        LOGICAL                             :: check

        indices = 0
        check = .TRUE.

        ! Create integer vector with ncol elements
        DO i = 1, ncol
            xlindgen(i) = i
        END DO 
        
        DO WHILE ( check )
            ! Get an array of random numbers using RANDOM_NUMBER, which
            ! Returns a single pseudorandom number or an array of pseudorandom
            ! numbers from the uniform distribution over the range 0 < x < 1.
            rndRealArr = 0.0
            CALL RANDOM_NUMBER( rndRealArr )
            check = RANDOM_NUMBER_DUPLICATES( ncol, rndRealArr )
            !IF ( check ) PRINT*, "-- WARNING: dupliate random number -> try again!"
        END DO

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

    SUBROUTINE PSEUDO_RETRIEVAL( thv, flag, ncol, nlev, x, y, &
                                 inp, matrix, array )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        INTEGER(KIND=sint),  INTENT(IN)    :: ncol, nlev, flag
        INTEGER(KIND=sint),  INTENT(IN)    :: x, y
        REAL(KIND=sreal),    INTENT(IN)    :: thv
        TYPE(era_input),     INTENT(IN)    :: inp
        TYPE(scops_matrix),  INTENT(INOUT) :: matrix
        TYPE(pseudo_arrays), INTENT(INOUT) :: array

        ! local variables
        INTEGER(KIND=sint) :: icol, ilev
        REAL(KIND=sreal)   :: scale_factor


        WHERE ( matrix % tcot < thv )
            matrix % cfc =  0.0
            matrix % cph = -1.0
        END WHERE

        IF ( flag == is_day ) THEN
            WHERE ( matrix % tcot < thv) 
                matrix % cot =  0.0
                matrix % cer = -1.0
                matrix % cwp =  0.0
            END WHERE
        END IF


        ! search cloud top: nlev=1=top, nlev=26=bottom
        col: DO icol = 1, ncol
            lev: DO ilev = 1, nlev

                IF ( matrix % cfc (icol, ilev) > 0.5 ) THEN

                    array % ctp (icol) = inp % plevel (ilev) / 100.
                    array % cth (icol) = inp % geop (x,y,ilev) / 9.81
                    array % ctt (icol) = inp % temp (x,y,ilev)
                    array % cph (icol) = NINT( matrix % cph (icol, ilev) ) 
                    array % cfc (icol) = 1.0

                    IF ( flag == is_day ) THEN

                        array % cer (icol) = matrix % cer (icol, ilev)
                        array % cot (icol) = SUM( matrix % cot (icol,:) )
                        array % cwp (icol) = SUM( matrix % cwp (icol,:) )

                        IF ( array % cot (icol) > max_cot ) THEN
                            scale_factor = max_cot / array % cot (icol)
                            array % cwp (icol) = array % cwp (icol) * scale_factor
                            array % cot (icol) = array % cot (icol) * scale_factor
                        END IF

                    END IF

                    CYCLE col

                END IF

            END DO lev
        END DO col

    END SUBROUTINE PSEUDO_RETRIEVAL

    !==========================================================================

    SUBROUTINE COMPUTE_SUMMARY_STATISTICS( x, y, ncol, flag, &
                                           array, set, tmp, fin )

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE FUNCS, only: GET_MEAN

        IMPLICIT NONE

        INTEGER(KIND=sint),  INTENT(IN) :: x, y, flag, ncol
        TYPE(config),        INTENT(IN) :: set   ! histo settings
        TYPE(pseudo_arrays), INTENT(IN) :: array ! ncol-vectors for grid cell
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp   ! tmp gridbox results
        TYPE(l3_vars),    INTENT(INOUT) :: fin   ! collecting hist-results

        ! local variable
        INTEGER(KIND=sint)                :: i
        REAL(KIND=sreal), DIMENSION(ncol) :: all_phase
        REAL(KIND=sreal), DIMENSION(ncol) :: liq_phase, ice_phase


        ! create phase masks for mean calculation
        all_phase = array % cph
        liq_phase = array % cph
        ice_phase = array % cph
        WHERE ( array % cfc == is_clear ) all_phase = sreal_fill_value
        WHERE ( array % cph == is_ice )   liq_phase = sreal_fill_value
        WHERE ( array % cph == is_liq )   ice_phase = sreal_fill_value


        ! get all_phase parameters
        tmp % cfc (x,y) = GET_MEAN( ncol, array % cfc, all_phase, allsky )
        IF ( tmp % cfc (x,y) == sreal_fill_value ) tmp % cfc (x,y) = 0.0

        tmp % ctp (x,y) = GET_MEAN( ncol, array % ctp, all_phase, normal )
        tmp % cth (x,y) = GET_MEAN( ncol, array % cth, all_phase, normal )
        tmp % ctt (x,y) = GET_MEAN( ncol, array % ctt, all_phase, normal )
        tmp % cph (x,y) = GET_MEAN( ncol, array % cph, all_phase, normal )

        IF ( flag == is_day ) THEN
            tmp % cph_day (x,y) = tmp % cph (x,y)
            tmp % cot (x,y) = GET_MEAN( ncol, array % cot, all_phase, normal )
            tmp % cer (x,y) = GET_MEAN( ncol, array % cer, all_phase, normal ) 
            tmp % cwp (x,y) = GET_MEAN( ncol, array % cwp, all_phase, normal )
            tmp % cwp_allsky (x,y) = GET_MEAN( ncol, array % cwp, &
                                               all_phase, allsky )
        ELSE ! night
            tmp % cph_day (x,y) = sreal_fill_value
            tmp % cot (x,y) = sreal_fill_value
            tmp % cer (x,y) = sreal_fill_value
            tmp % cwp (x,y) = sreal_fill_value
            tmp % cwp_allsky (x,y) = sreal_fill_value
        END IF


        ! get liquid parameters
        IF ( flag == is_day ) THEN
            tmp % lwp (x,y) = GET_MEAN( ncol, array % cwp, liq_phase, normal )
            tmp % lwp_allsky (x,y) = GET_MEAN( ncol, array % cwp, liq_phase, allsky)
            tmp % cot_liq (x,y) = GET_MEAN( ncol, array % cot, liq_phase, normal )
            tmp % cer_liq (x,y) = GET_MEAN( ncol, array % cer, liq_phase, normal )
        ELSE ! night
            tmp % lwp (x,y) = sreal_fill_value
            tmp % lwp_allsky (x,y) = sreal_fill_value
            tmp % cot_liq (x,y) = sreal_fill_value
            tmp % cer_liq (x,y) = sreal_fill_value
        END IF


        ! get ice parameters
        IF ( flag == is_day ) THEN
            tmp % iwp (x,y) = GET_MEAN( ncol, array % cwp, ice_phase, normal )
            tmp % iwp_allsky (x,y) = GET_MEAN( ncol, array % cwp, ice_phase, allsky)
            tmp % cot_ice (x,y) = GET_MEAN( ncol, array % cot, ice_phase, normal )
            tmp % cer_ice (x,y) = GET_MEAN( ncol, array % cer, ice_phase, normal )
        ELSE ! night
            tmp % iwp (x,y) = sreal_fill_value
            tmp % iwp_allsky (x,y) = sreal_fill_value
            tmp % cot_ice (x,y) = sreal_fill_value
            tmp % cer_ice (x,y) = sreal_fill_value
        END IF


        ! collect histogram information

        hist: DO i=1, ncol

            IF ( array % cfc(i) == is_clear ) CYCLE hist

            CALL MAKE_1D_HISTOGRAM( "ctp", array % ctp(i),  &
                                    array % cph(i), x, y,   &
                                    set % hist_ctp_1d_axis, &
                                    fin % hist_ctp )

            CALL MAKE_1D_HISTOGRAM( "ctt", array % ctt(i),  &
                                    array % cph(i), x, y,   &
                                    set % hist_ctt_1d_axis, &
                                    fin % hist_ctt )

            IF ( flag == is_day ) THEN 

                CALL MAKE_2D_HISTOGRAM( array % cot(i), array %ctp(i), &
                                        array % cph(i), x, y, &
                                        set % hist_cot_2d_axis, &
                                        set % hist_ctp_2d_axis, &
                                        fin % hist_cot_ctp )

                CALL MAKE_1D_HISTOGRAM( "cot", array % cot(i),  &
                                        array % cph(i), x, y,   &
                                        set % hist_cot_1d_axis, &
                                        fin % hist_cot )

                CALL MAKE_1D_HISTOGRAM( "cer", array % cer(i),  &
                                        array % cph(i), x, y,   &
                                        set % hist_cer_1d_axis, &
                                        fin % hist_cer )

                CALL MAKE_1D_HISTOGRAM( "cwp", array % cwp(i)*1000.,  &
                                        array % cph(i), x, y,   &
                                        set % hist_cwp_1d_axis, &
                                        fin % hist_cwp )

            END IF

        END DO hist


    END SUBROUTINE COMPUTE_SUMMARY_STATISTICS
    
    !==========================================================================
    
    SUBROUTINE MAKE_1D_HISTOGRAM( what, value, phase, xi, yj, axis, res )

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        CHARACTER(LEN=3),   INTENT(IN)    :: what
        INTEGER(KIND=sint), INTENT(IN)    :: xi, yj
        REAL(KIND=sreal),   INTENT(IN)    :: value
        REAL(KIND=sreal),   INTENT(IN)    :: phase
        REAL(KIND=sreal),   INTENT(IN)    :: axis(:)
        INTEGER(KIND=lint), INTENT(INOUT) :: res(:,:,:,:)

        ! local variable
        INTEGER(KIND=sint) :: jp, jval, n_axis, n_bins

        SELECT CASE (what)
           CASE ("ctt")
              n_bins = n_ctt_bins
              n_axis = n_ctt_bins + 1
           CASE ("ctp")
              n_bins = n_ctp_bins
              n_axis = n_ctp_bins + 1
           CASE ("cot")
              n_bins = n_cot_bins
              n_axis = n_cot_bins + 1
           CASE ("cwp")
              n_bins = n_cwp_bins
              n_axis = n_cwp_bins + 1
           CASE ("cer")
              n_bins = n_cer_bins
              n_axis = n_cer_bins + 1
           CASE DEFAULT
              print*, " --- ERROR: This case is not defined: ", what
              STOP
        END SELECT

        IF ( phase == is_liq ) THEN
            jp = liq_bin
        ELSE IF ( phase == is_ice ) THEN 
            jp = ice_bin
        ELSE 
            RETURN
        END IF

        CALL LOCATE( axis, n_axis, value, jval )

        IF ( jval  < 1 ) jval = 1
        IF ( jval == n_axis ) jval = n_bins

        res(xi,yj,jval,jp) = res(xi,yj,jval,jp) + 1

    END SUBROUTINE MAKE_1D_HISTOGRAM

    !==========================================================================

    SUBROUTINE MAKE_2D_HISTOGRAM( cot, ctp, phase, xi, yj, &
                                  cot_axis, ctp_axis, res )

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=sint), INTENT(IN)    :: xi, yj
        REAL(KIND=sreal),   INTENT(IN)    :: cot, ctp
        REAL(KIND=sreal),   INTENT(IN)    :: phase
        REAL(KIND=sreal),   INTENT(IN)    :: cot_axis(:)
        REAL(KIND=sreal),   INTENT(IN)    :: ctp_axis(:)
        INTEGER(KIND=lint), INTENT(INOUT) :: res(:,:,:,:,:)

        ! local variable
        INTEGER(KIND=sint) :: jp, jcot, jctp

        IF ( phase == is_liq ) THEN
            jp = liq_bin
        ELSE IF ( phase == is_ice ) THEN 
            jp = ice_bin
        ELSE 
            RETURN
        END IF

        CALL LOCATE( cot_axis, n_hist_cot, cot, jcot )
        CALL LOCATE( ctp_axis, n_hist_ctp, ctp, jctp )

        IF ( jcot < 1 ) jcot = 1
        IF ( jcot == n_hist_cot ) jcot = n_hist_cot - 1

        IF ( jctp < 1 ) jctp = 1
        IF ( jctp == n_hist_ctp ) jctp = n_hist_ctp - 1

        res(xi,yj,jcot,jctp,jp) = res(xi,yj,jcot,jctp,jp) + 1

    END SUBROUTINE MAKE_2D_HISTOGRAM

    !==========================================================================

    SUBROUTINE LOCATE( xx, n, x, j )
      
        USE COMMON_CONSTANTS
        
        IMPLICIT NONE
        
        INTEGER(KIND=sint), INTENT(IN)  :: n
        REAL(KIND=sreal),   INTENT(IN)  :: x, xx(n)
        INTEGER(KIND=sint), INTENT(OUT) :: j

        ! local variables
        INTEGER(KIND=sint) :: jl, jm, ju

        jl = 0
        ju = n + 1

    10  if (ju-jl .gt. 1) then

          jm = ( ju+jl ) / 2

          if ( (xx(n) .ge. xx(1)) .eqv. (x.ge.xx(jm)) ) then
             jl = jm
          else
             ju = jm
          endif

          goto 10

        endif

        if ( x .eq. xx(1) ) then
           j = 1
        else if ( x .eq. xx(n) ) then
           j = n-1
        else
           j = jl
        endif

    END SUBROUTINE LOCATE

    !==========================================================================

    ! from mod4sim.F90 (Salomon Eliasson)
    SUBROUTINE GET_SEED(era_temp, nlev, nps, seed, rngs)

        USE COMMON_CONSTANTS, ONLY: sint, sreal
        USE cosp_kinds, ONLY: wp
        USE mod_rng,    ONLY: rng_state

        INTEGER(KIND=sint),                         INTENT(IN)  :: nlev
        INTEGER(KIND=sint),                         INTENT(IN)  :: nps
        REAL(KIND=sreal), DIMENSION(nlev+1),        INTENT(IN)  :: era_temp
        INTEGER, DIMENSION(:), ALLOCATABLE,         INTENT(OUT) :: seed
        TYPE(rng_state), ALLOCATABLE, DIMENSION(:), INTENT(OUT) :: rngs

        ! local
        REAL(wp), DIMENSION(nlev) :: temperature

        ALLOCATE( seed(nps) )
        ALLOCATE( rngs(nps) )

        seed(:) = 0
        temperature = era_temp(1:nlev+1-1)*0.5 + era_temp(2:nlev+1)*0.5
        seed = INT( temperature(1)*10.0 )

    END SUBROUTINE GET_SEED

    !==========================================================================

    SUBROUTINE COSP_SUBCOLS( ncol, nlev, flag, npoints_m, seed, rngs, &
                           overlap, icc, lcot, icot, lcer, icer, lwp, iwp, &
                           matrix )

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE cosp_kinds, ONLY: wp
        USE mod_scops,  ONLY: scops
        USE mod_rng,    ONLY: rng_state

        IMPLICIT NONE

        INTEGER(KIND=sint),                    INTENT(IN) :: overlap
        INTEGER(KIND=sint),                    INTENT(IN) :: npoints_m
        INTEGER(KIND=sint),                    INTENT(IN) :: flag, nlev, ncol
        INTEGER, DIMENSION(npoints_m),         INTENT(IN) :: seed
        TYPE(rng_state), DIMENSION(npoints_m), INTENT(IN) :: rngs
        REAL(KIND=sreal), DIMENSION(nlev),     INTENT(IN) :: lcot, icot
        REAL(KIND=sreal), DIMENSION(nlev),     INTENT(IN) :: lcer, icer
        REAL(KIND=sreal), DIMENSION(nlev),     INTENT(IN) :: lwp, iwp
        REAL(KIND=sreal), DIMENSION(nlev+1),   INTENT(IN) :: icc
        TYPE(scops_matrix),                 INTENT(INOUT) :: matrix

        ! local variables
        INTEGER(KIND=sint)                :: i,j, zi, ci
        REAL(KIND=sreal), DIMENSION(nlev) :: cwp_all, cot_all
        REAL(wp), DIMENSION(:),   ALLOCATABLE :: cc, cv
        REAL(wp), DIMENSION(:,:), ALLOCATABLE :: frac_out

        cwp_all(1:nlev) =  lwp(1:nlev) +  iwp(1:nlev)
        cot_all(1:nlev) = lcot(1:nlev) + icot(1:nlev)

        ALLOCATE( cc(nlev) )
        ALLOCATE( cv(nlev) )
        ALLOCATE( frac_out(ncol,nlev) )

        frac_out(1:ncol,1:nlev) = 0.0

        cv(1:nlev) = 0.0 ! convective cloud cover is zero
        cc(1:nlev) = 0.0 ! cloud cover
        cc(1:nlev) = icc(1:nlev+1-1) * 0.5 + icc(2:nlev+1) * 0.5

        CALL SCOPS( npoints_m, nlev, ncol, seed, rngs, &
                    cc, cv, overlap, frac_out, 0 )

        DO zi=1, nlev !loop over model levels 
            DO ci=1, ncol !loop over subcolumns

                IF ( frac_out(ci,zi) == 1.0 ) THEN

                    matrix % cfc (ci,zi) = 1.0
                    matrix % cot (ci,zi) = cot_all(zi)
                    
                    IF ( flag == is_day ) matrix % cwp (ci,zi) = cwp_all(zi)

                    IF ( cwp_all(zi) > 0 ) THEN 
                        !liquid fraction
                        matrix % cph (ci,zi) = lwp(zi) / cwp_all(zi)

                        !weighted mean CER day only
                        IF ( flag == is_day ) matrix % cer (ci,zi) = &
                            ( lcer(zi)*lwp(zi) + icer(zi)*iwp(zi) ) / cwp_all(zi)
                    ENDIF

                END IF

            END DO !end loop ci-subcol

            !TOP = upper-most layer: zi=1
            IF ( zi == 1 ) matrix % tcot(:,zi) = matrix % cot(:,1) 
            IF ( zi  > 1 ) matrix % tcot(:,zi) = SUM( matrix % cot(:,1:zi), 2 )

        END DO !end loop zi-level

        DEALLOCATE( cc )
        DEALLOCATE( cv )
        DEALLOCATE( frac_out )

    END SUBROUTINE COSP_SUBCOLS

    !==========================================================================

END MODULE SIM_CORE
