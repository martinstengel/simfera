!-------------------------------------------------------------------------------
! Name: undefine.F90
!-------------------------------------------------------------------------------
MODULE UNDEFINE 

    CONTAINS

    !==========================================================================
    ! DEALLOCATE ARRAYS
    !==========================================================================

    SUBROUTINE UNDEFINE_INPUT( input )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(era_input), INTENT(INOUT) :: input

        IF ( ALLOCATED(input % lon)   ) DEALLOCATE(input % lon)
        IF ( ALLOCATED(input % lat)   ) DEALLOCATE(input % lat)
        IF ( ALLOCATED(input % plevel)) DEALLOCATE(input % plevel)
        IF ( ALLOCATED(input % dpres) ) DEALLOCATE(input % dpres)
        IF ( ALLOCATED(input % cc)    ) DEALLOCATE(input % cc)
        IF ( ALLOCATED(input % lwc)   ) DEALLOCATE(input % lwc)
        IF ( ALLOCATED(input % iwc)   ) DEALLOCATE(input % iwc)
        IF ( ALLOCATED(input % geop)  ) DEALLOCATE(input % geop)
        IF ( ALLOCATED(input % temp)  ) DEALLOCATE(input % temp)
        IF ( ALLOCATED(input % sza2d) ) DEALLOCATE(input % sza2d)

    END SUBROUTINE UNDEFINE_INPUT

    !==========================================================================

    SUBROUTINE UNDEFINE_TEMPS( temps )

        USE STRUCTS
        IMPLICIT NONE
        TYPE(tmp_arrays), INTENT(INOUT) :: temps

        IF ( ALLOCATED (temps % lwc_inc) )    DEALLOCATE(temps % lwc_inc)
        IF ( ALLOCATED (temps % iwc_inc) )    DEALLOCATE(temps % iwc_inc)
        IF ( ALLOCATED (temps % lwp_lay) )    DEALLOCATE(temps % lwp_lay)
        IF ( ALLOCATED (temps % iwp_lay) )    DEALLOCATE(temps % iwp_lay)
        IF ( ALLOCATED (temps % lcer_lay) )   DEALLOCATE(temps % lcer_lay)
        IF ( ALLOCATED (temps % icer_lay) )   DEALLOCATE(temps % icer_lay)
        IF ( ALLOCATED (temps % lcot_lay) )   DEALLOCATE(temps % lcot_lay)
        IF ( ALLOCATED (temps % icot_lay) )   DEALLOCATE(temps % icot_lay)
        IF ( ALLOCATED (temps % cfc) )        DEALLOCATE(temps % cfc)
        IF ( ALLOCATED (temps % cph) )        DEALLOCATE(temps % cph)
        IF ( ALLOCATED (temps % cph_day) )    DEALLOCATE(temps % cph_day)
        IF ( ALLOCATED (temps % ctp) )        DEALLOCATE(temps % ctp)
        IF ( ALLOCATED (temps % cth) )        DEALLOCATE(temps % cth)
        IF ( ALLOCATED (temps % ctt) )        DEALLOCATE(temps % ctt)
        IF ( ALLOCATED (temps % cwp) )        DEALLOCATE(temps % cwp)
        IF ( ALLOCATED (temps % lwp) )        DEALLOCATE(temps % lwp)
        IF ( ALLOCATED (temps % iwp) )        DEALLOCATE(temps % iwp)
        IF ( ALLOCATED (temps % cwp_allsky) ) DEALLOCATE(temps % cwp_allsky)
        IF ( ALLOCATED (temps % lwp_allsky) ) DEALLOCATE(temps % lwp_allsky)
        IF ( ALLOCATED (temps % iwp_allsky) ) DEALLOCATE(temps % iwp_allsky)
        IF ( ALLOCATED (temps % cot) )        DEALLOCATE(temps % cot)
        IF ( ALLOCATED (temps % cot_liq) )    DEALLOCATE(temps % cot_liq)
        IF ( ALLOCATED (temps % cot_ice) )    DEALLOCATE(temps % cot_ice)
        IF ( ALLOCATED (temps % cer) )        DEALLOCATE(temps % cer)
        IF ( ALLOCATED (temps % cer_liq) )    DEALLOCATE(temps % cer_liq)
        IF ( ALLOCATED (temps % cer_ice) )    DEALLOCATE(temps % cer_ice)

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
        IF ( ALLOCATED (final % cph)          ) DEALLOCATE(final % cph)
        IF ( ALLOCATED (final % cph_day)      ) DEALLOCATE(final % cph_day)
        IF ( ALLOCATED (final % ctp)          ) DEALLOCATE(final % ctp)
        IF ( ALLOCATED (final % cth)          ) DEALLOCATE(final % cth)
        IF ( ALLOCATED (final % ctt)          ) DEALLOCATE(final % ctt)
        IF ( ALLOCATED (final % cwp)          ) DEALLOCATE(final % cwp)
        IF ( ALLOCATED (final % lwp)          ) DEALLOCATE(final % lwp)
        IF ( ALLOCATED (final % iwp)          ) DEALLOCATE(final % iwp)
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


END MODULE UNDEFINE
