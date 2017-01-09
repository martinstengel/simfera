!-------------------------------------------------------------------------------
! Name: undefine.F90
!-------------------------------------------------------------------------------
MODULE UNDEFINE 

    CONTAINS

    !==========================================================================
    ! DEALLOCATE ARRAYS
    !==========================================================================

    SUBROUTINE UNDEFINE_ARRAYS( array )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(pseudo_arrays), INTENT(INOUT) :: array

        IF ( ALLOCATED(array % ctp) )  DEALLOCATE( array % ctp ) 
        IF ( ALLOCATED(array % cth) )  DEALLOCATE( array % cth )
        IF ( ALLOCATED(array % ctt) )  DEALLOCATE( array % ctt )
        IF ( ALLOCATED(array % cph) )  DEALLOCATE( array % cph )
        IF ( ALLOCATED(array % cot) )  DEALLOCATE( array % cot )
        IF ( ALLOCATED(array % cwp) )  DEALLOCATE( array % cwp )
        IF ( ALLOCATED(array % mlwp) ) DEALLOCATE( array % mlwp )
        IF ( ALLOCATED(array % miwp) ) DEALLOCATE( array % miwp )
        IF ( ALLOCATED(array % cer) )  DEALLOCATE( array % cer )
        IF ( ALLOCATED(array % cfc) )  DEALLOCATE( array % cfc )
        IF ( ALLOCATED(array % cfc_high) ) DEALLOCATE( array % cfc_high )
        IF ( ALLOCATED(array % cfc_mid) )  DEALLOCATE( array % cfc_mid )
        IF ( ALLOCATED(array % cfc_low) )  DEALLOCATE( array % cfc_low )

    END SUBROUTINE UNDEFINE_ARRAYS

    !==========================================================================

    SUBROUTINE UNDEFINE_MATRIX( matrix )
    
        USE STRUCTS
        IMPLICIT NONE
        TYPE(scops_matrix), INTENT(INOUT) :: matrix

        IF ( ALLOCATED(matrix % tcot)) DEALLOCATE( matrix % tcot )
        IF ( ALLOCATED(matrix % cot) ) DEALLOCATE( matrix % cot  )
        IF ( ALLOCATED(matrix % cfc) ) DEALLOCATE( matrix % cfc  )
        IF ( ALLOCATED(matrix % cwp) ) DEALLOCATE( matrix % cwp  )
        IF ( ALLOCATED(matrix % cph) ) DEALLOCATE( matrix % cph  )
        IF ( ALLOCATED(matrix % cer) ) DEALLOCATE( matrix % cer  )

    END SUBROUTINE UNDEFINE_MATRIX

    !==========================================================================

    SUBROUTINE UNDEFINE_INPUT( input )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(era_input), INTENT(INOUT) :: input

        IF ( ALLOCATED(input % lon)   )      DEALLOCATE(input % lon)
        IF ( ALLOCATED(input % lat)   )      DEALLOCATE(input % lat)
        IF ( ALLOCATED(input % lev)   )      DEALLOCATE(input % lev)
        IF ( ALLOCATED(input % sza2d) )      DEALLOCATE(input % sza2d)
        IF ( ALLOCATED(input % lnsp2d) )     DEALLOCATE(input % lnsp2d)
        IF ( ALLOCATED(input % geop2d) )     DEALLOCATE(input % geop2d)
        IF ( ALLOCATED(input % cc_prof)   )  DEALLOCATE(input % cc_prof)
        IF ( ALLOCATED(input % lwc_prof)  )  DEALLOCATE(input % lwc_prof)
        IF ( ALLOCATED(input % iwc_prof)  )  DEALLOCATE(input % iwc_prof)
        IF ( ALLOCATED(input % temp_prof) )  DEALLOCATE(input % temp_prof)
        IF ( ALLOCATED(input % shum_prof) )  DEALLOCATE(input % shum_prof)
        IF ( ALLOCATED(input % geop_prof) )  DEALLOCATE(input % geop_prof)
        IF ( ALLOCATED(input % pres_prof) )  DEALLOCATE(input % pres_prof)
        IF ( ALLOCATED(input % dpres_prof) ) DEALLOCATE(input % dpres_prof)

    END SUBROUTINE UNDEFINE_INPUT

    !==========================================================================

    SUBROUTINE UNDEFINE_TEMPS( tmp )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp

        IF ( ALLOCATED (tmp % lwc_prof_inc) ) DEALLOCATE(tmp % lwc_prof_inc)
        IF ( ALLOCATED (tmp % iwc_prof_inc) ) DEALLOCATE(tmp % iwc_prof_inc)
        IF ( ALLOCATED (tmp % lwp_prof) )     DEALLOCATE(tmp % lwp_prof)
        IF ( ALLOCATED (tmp % iwp_prof) )     DEALLOCATE(tmp % iwp_prof)
        IF ( ALLOCATED (tmp % lcer_prof) )    DEALLOCATE(tmp % lcer_prof)
        IF ( ALLOCATED (tmp % icer_prof) )    DEALLOCATE(tmp % icer_prof)
        IF ( ALLOCATED (tmp % lcot_prof) )    DEALLOCATE(tmp % lcot_prof)
        IF ( ALLOCATED (tmp % icot_prof) )    DEALLOCATE(tmp % icot_prof)
        IF ( ALLOCATED (tmp % cfc) )          DEALLOCATE(tmp % cfc)
        IF ( ALLOCATED (tmp % cfc_high) )     DEALLOCATE(tmp % cfc_high)
        IF ( ALLOCATED (tmp % cfc_mid) )      DEALLOCATE(tmp % cfc_mid)
        IF ( ALLOCATED (tmp % cfc_low) )      DEALLOCATE(tmp % cfc_low)
        IF ( ALLOCATED (tmp % cph) )          DEALLOCATE(tmp % cph)
        IF ( ALLOCATED (tmp % cph_day) )      DEALLOCATE(tmp % cph_day)
        IF ( ALLOCATED (tmp % ctp) )          DEALLOCATE(tmp % ctp)
        IF ( ALLOCATED (tmp % cth) )          DEALLOCATE(tmp % cth)
        IF ( ALLOCATED (tmp % ctt) )          DEALLOCATE(tmp % ctt)
        IF ( ALLOCATED (tmp % cwp) )          DEALLOCATE(tmp % cwp)
        IF ( ALLOCATED (tmp % lwp) )          DEALLOCATE(tmp % lwp)
        IF ( ALLOCATED (tmp % iwp) )          DEALLOCATE(tmp % iwp)
        IF ( ALLOCATED (tmp % mlwp) )         DEALLOCATE(tmp % mlwp)
        IF ( ALLOCATED (tmp % miwp) )         DEALLOCATE(tmp % miwp)
        IF ( ALLOCATED (tmp % cwp_allsky) )   DEALLOCATE(tmp % cwp_allsky)
        IF ( ALLOCATED (tmp % lwp_allsky) )   DEALLOCATE(tmp % lwp_allsky)
        IF ( ALLOCATED (tmp % iwp_allsky) )   DEALLOCATE(tmp % iwp_allsky)
        IF ( ALLOCATED (tmp % cot) )          DEALLOCATE(tmp % cot)
        IF ( ALLOCATED (tmp % cot_liq) )      DEALLOCATE(tmp % cot_liq)
        IF ( ALLOCATED (tmp % cot_ice) )      DEALLOCATE(tmp % cot_ice)
        IF ( ALLOCATED (tmp % cer) )          DEALLOCATE(tmp % cer)
        IF ( ALLOCATED (tmp % cer_liq) )      DEALLOCATE(tmp % cer_liq)
        IF ( ALLOCATED (tmp % cer_ice) )      DEALLOCATE(tmp % cer_ice)

    END SUBROUTINE UNDEFINE_TEMPS

    !==========================================================================

    SUBROUTINE UNDEFINE_AUX( aux )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(era_aux), INTENT(INOUT) :: aux

        IF ( ALLOCATED(aux % lon)   ) DEALLOCATE(aux % lon)
        IF ( ALLOCATED(aux % lat)   ) DEALLOCATE(aux % lat)
        IF ( ALLOCATED(aux % sst2d) ) DEALLOCATE(aux % sst2d)
        IF ( ALLOCATED(aux % lsm2d) ) DEALLOCATE(aux % lsm2d)
        IF ( ALLOCATED(aux % lon2d) ) DEALLOCATE(aux % lon2d)
        IF ( ALLOCATED(aux % lat2d) ) DEALLOCATE(aux % lat2d)

    END SUBROUTINE UNDEFINE_AUX

    !==========================================================================


    SUBROUTINE UNDEFINE_FINAL( final )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(l3_vars), INTENT(INOUT) :: final

        IF ( ALLOCATED (final % cfc)          ) DEALLOCATE(final % cfc)
        IF ( ALLOCATED (final % cfc_high)     ) DEALLOCATE(final % cfc_high)
        IF ( ALLOCATED (final % cfc_mid)      ) DEALLOCATE(final % cfc_mid)
        IF ( ALLOCATED (final % cfc_low)      ) DEALLOCATE(final % cfc_low)
        IF ( ALLOCATED (final % cph)          ) DEALLOCATE(final % cph)
        IF ( ALLOCATED (final % cph_day)      ) DEALLOCATE(final % cph_day)
        IF ( ALLOCATED (final % ctp)          ) DEALLOCATE(final % ctp)
        IF ( ALLOCATED (final % cth)          ) DEALLOCATE(final % cth)
        IF ( ALLOCATED (final % ctt)          ) DEALLOCATE(final % ctt)
        IF ( ALLOCATED (final % cwp)          ) DEALLOCATE(final % cwp)
        IF ( ALLOCATED (final % lwp)          ) DEALLOCATE(final % lwp)
        IF ( ALLOCATED (final % iwp)          ) DEALLOCATE(final % iwp)
        IF ( ALLOCATED (final % mlwp)         ) DEALLOCATE(final % mlwp)
        IF ( ALLOCATED (final % miwp)         ) DEALLOCATE(final % miwp)
        IF ( ALLOCATED (final % cwp_allsky)   ) DEALLOCATE(final % cwp_allsky)
        IF ( ALLOCATED (final % lwp_allsky)   ) DEALLOCATE(final % lwp_allsky)
        IF ( ALLOCATED (final % iwp_allsky)   ) DEALLOCATE(final % iwp_allsky)
        IF ( ALLOCATED (final % cot)          ) DEALLOCATE(final % cot)
        IF ( ALLOCATED (final % cot_liq)      ) DEALLOCATE(final % cot_liq)
        IF ( ALLOCATED (final % cot_ice)      ) DEALLOCATE(final % cot_ice)
        IF ( ALLOCATED (final % cer)          ) DEALLOCATE(final % cer)
        IF ( ALLOCATED (final % cer_liq)      ) DEALLOCATE(final % cer_liq)
        IF ( ALLOCATED (final % cer_ice)      ) DEALLOCATE(final % cer_ice)
        IF ( ALLOCATED (final % hist_cot_ctp) ) DEALLOCATE(final % hist_cot_ctp)
        IF ( ALLOCATED (final % hist_ctp)     ) DEALLOCATE(final % hist_ctp)
        IF ( ALLOCATED (final % hist_ctt)     ) DEALLOCATE(final % hist_ctt)
        IF ( ALLOCATED (final % hist_cot)     ) DEALLOCATE(final % hist_cot)
        IF ( ALLOCATED (final % hist_cwp)     ) DEALLOCATE(final % hist_cwp)
        IF ( ALLOCATED (final % hist_cer)     ) DEALLOCATE(final % hist_cer)

    END SUBROUTINE UNDEFINE_FINAL

    !==========================================================================

    SUBROUTINE UNDEFINE_COUNTS( cnt )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(npoints), INTENT(INOUT) :: cnt

        cnt % file_counter = 0

        IF ( ALLOCATED (cnt % cfc) )        DEALLOCATE( cnt % cfc )
        IF ( ALLOCATED (cnt % ctp) )        DEALLOCATE( cnt % ctp )
        IF ( ALLOCATED (cnt % cwp) )        DEALLOCATE( cnt % cwp )
        IF ( ALLOCATED (cnt % lwp) )        DEALLOCATE( cnt % lwp )
        IF ( ALLOCATED (cnt % iwp) )        DEALLOCATE( cnt % iwp )
        IF ( ALLOCATED (cnt % mlwp) )       DEALLOCATE( cnt % mlwp )
        IF ( ALLOCATED (cnt % miwp) )       DEALLOCATE( cnt % miwp )
        IF ( ALLOCATED (cnt % cwp_allsky) ) DEALLOCATE( cnt % cwp_allsky )
        IF ( ALLOCATED (cnt % lwp_allsky) ) DEALLOCATE( cnt % lwp_allsky )
        IF ( ALLOCATED (cnt % iwp_allsky) ) DEALLOCATE( cnt % iwp_allsky )
        IF ( ALLOCATED (cnt % cot) )        DEALLOCATE( cnt % cot )
        IF ( ALLOCATED (cnt % cot_liq) )    DEALLOCATE( cnt % cot_liq )
        IF ( ALLOCATED (cnt % cot_ice) )    DEALLOCATE( cnt % cot_ice )
        IF ( ALLOCATED (cnt % cer) )        DEALLOCATE( cnt % cer )
        IF ( ALLOCATED (cnt % cer_liq) )    DEALLOCATE( cnt % cer_liq )
        IF ( ALLOCATED (cnt % cer_ice) )    DEALLOCATE( cnt % cer_ice )
        IF ( ALLOCATED (cnt % cph_day) )    DEALLOCATE( cnt % cph_day )

    END SUBROUTINE UNDEFINE_COUNTS

    !==========================================================================

END MODULE UNDEFINE
