!-------------------------------------------------------------------------------
! Name: sim_ncdf.F90
!-------------------------------------------------------------------------------
MODULE SIM_NCDF 

    USE COMMON_CONSTANTS
    USE NCDF_CONSTANTS
    USE STRUCTS
    USE NETCDF

    CONTAINS

    !==========================================================================

    SUBROUTINE DEFINE_VAR( fid, vartype, short, dim_var, dimid, varid )

        IMPLICIT NONE

        INTEGER,           INTENT(IN)  :: fid, vartype, dim_var
        CHARACTER(LEN=64), INTENT(IN)  :: short
        INTEGER,           INTENT(OUT) :: varid, dimid

        CALL CHECK ( nf90_def_dim( fid, short, dim_var, dimid ) )

        IF ( vartype == 1 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_INT, dimid, varid ) )
        ELSE IF ( vartype == 2 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_FLOAT, dimid, varid ) )
        ELSE IF ( vartype == 3 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_DOUBLE, dimid, varid ) )
        ELSE IF ( vartype == 4 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_BYTE, dimid, varid ) )
        ELSE
            PRINT*, " --- ERROR: This vartype is not defined !", vartype
            STOP
        END IF

    END SUBROUTINE DEFINE_VAR

    !==========================================================================

    SUBROUTINE DEFINE_OUT( fid, vartype, short, dimids, varid )

        IMPLICIT NONE

        INTEGER,           INTENT(IN)  :: fid, vartype, dimids(:)
        CHARACTER(LEN=64), INTENT(IN)  :: short
        INTEGER,           INTENT(OUT) :: varid

        IF ( vartype == 1 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_INT, dimids, varid ) )
        ELSE IF ( vartype == 2 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_FLOAT, dimids, varid ) )
        ELSE IF ( vartype == 3 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_DOUBLE, dimids, varid ) )
        ELSE
            PRINT*, " --- ERROR: This vartype is not defined !", vartype
            STOP
        END IF

    END SUBROUTINE DEFINE_OUT

    !==========================================================================

    SUBROUTINE DEFINE_ATTRIBUTES( fid, phase_id, h2ids, h1ids, vids )

        IMPLICIT NONE

        INTEGER,          INTENT(IN) :: fid, phase_id
        TYPE(hist2d_ids), INTENT(IN) :: h2ids
        TYPE(hist1d_ids), INTENT(IN) :: h1ids
        TYPE(mm_ids),     INTENT(IN) :: vids

        CALL CHECK ( nf90_put_att( fid, phase_id, long, phase_bins_str ) )
        CALL CHECK ( nf90_put_att( fid, phase_id, units, unit_one ) )

        CALL CHECK ( nf90_put_att( fid, h2ids % cot_axis, long, "cot"//axis_str ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % cot_axis, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % cot_bins, long, "cot"//bins_str ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % cot_bins, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % ctp_axis, long, "ctp"//axis_str ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % ctp_axis, units, unit_ctp ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % ctp_bins, long, "ctp"//bins_str ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % ctp_bins, units, unit_ctp ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % hist, long, jointstr ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % hist, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h2ids % hist, fill, lint_fill_value ) )

        CALL CHECK ( nf90_put_att( fid, h1ids % cot_axis, long, "cot"//axis_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cot_axis, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cot_bins, long, "cot"//bins_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cot_bins, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cot_hist, long, cot_hist_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cot_hist, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cot_hist, fill, lint_fill_value ) )

        CALL CHECK ( nf90_put_att( fid, h1ids % ctp_axis, long, "ctp"//axis_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctp_axis, units, unit_ctp ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctp_bins, long, "ctp"//bins_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctp_bins, units, unit_ctp ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctp_hist, long, ctp_hist_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctp_hist, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctp_hist, fill, lint_fill_value ) )

        CALL CHECK ( nf90_put_att( fid, h1ids % ctt_axis, long, "ctt"//axis_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctt_axis, units, unit_ctt ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctt_bins, long, "ctt"//bins_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctt_bins, units, unit_ctt ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctt_hist, long, ctt_hist_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctt_hist, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % ctt_hist, fill, lint_fill_value ) )

        CALL CHECK ( nf90_put_att( fid, h1ids % cer_axis, long, "cer"//axis_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cer_axis, units, unit_cer ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cer_bins, long, "cer"//bins_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cer_bins, units, unit_cer ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cer_hist, long, cer_hist_str) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cer_hist, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cer_hist, fill, lint_fill_value ) )

        CALL CHECK ( nf90_put_att( fid, h1ids % cwp_axis, long, "cwp"//axis_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cwp_axis, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cwp_bins, long, "cwp"//bins_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cwp_bins, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cwp_hist, long, cwp_hist_str ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cwp_hist, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, h1ids % cwp_hist, fill, lint_fill_value ) )

        CALL CHECK ( nf90_put_att( fid, vids % nobs, long, nobs_ctp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs, fill, lint_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs, fscale, 1 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs, offset, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs, vmin, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs, vmax, 124 ) ) 
        CALL CHECK ( nf90_put_att( fid, vids % nobs, COMMENT, nobs_comment ) ) 

        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, long, nobs_lwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, fill, lint_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, fscale, 1 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, offset, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, vmin, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, vmax, 124 ) ) 
        CALL CHECK ( nf90_put_att( fid, vids % nobs_lwp, COMMENT, nobs_comment ) ) 

        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, long, nobs_iwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, fill, lint_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, fscale, 1 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, offset, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, vmin, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, vmax, 124 ) ) 
        CALL CHECK ( nf90_put_att( fid, vids % nobs_iwp, COMMENT, nobs_comment ) ) 

        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, long, nobs_mlwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, fill, lint_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, fscale, 1 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, offset, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, vmin, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, vmax, 124 ) ) 
        CALL CHECK ( nf90_put_att( fid, vids % nobs_mlwp, COMMENT, nobs_comment ) ) 

        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, long, nobs_miwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, fill, lint_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, fscale, 1 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, offset, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, vmin, 0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, vmax, 124 ) ) 
        CALL CHECK ( nf90_put_att( fid, vids % nobs_miwp, COMMENT, nobs_comment ) ) 

        CALL CHECK ( nf90_put_att( fid, vids % cfc, long, cfc_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc, sname, cfc_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc, vmax, 1.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, long, cfc_high_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, vmax, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_high, COMMENT, cfc_high_com ) )

        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, long, cfc_mid_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, vmax, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_mid, COMMENT, cfc_mid_com ) )

        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, long, cfc_low_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, vmax, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cfc_low, COMMENT, cfc_low_com ) )

        CALL CHECK ( nf90_put_att( fid, vids % cph, long, cph_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph, vmax, 1.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cph_day, long, cph_day_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph_day, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph_day, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph_day, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph_day, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph_day, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cph_day, vmax, 1.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % ctp, long, ctp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctp, sname, ctp_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctp, units, unit_ctp ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctp, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctp, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctp, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctp, vmin, 50.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctp, vmax, 1200.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % ctt, long, ctt_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctt, sname, ctt_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctt, units, unit_ctt ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctt, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctt, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctt, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctt, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % ctt, vmax, 320.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cth, long, cth_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cth, sname, cth_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % cth, units, unit_cth ) )
        CALL CHECK ( nf90_put_att( fid, vids % cth, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cth, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cth, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cth, vmin, -1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cth, vmax, 20.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cot, long, cot_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot, sname, cot_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot, vmax, 320.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cot_liq, long, cot_liq_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_liq, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_liq, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_liq, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_liq, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_liq, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_liq, vmax, 320.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cot_ice, long, cot_ice_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_ice, units, unit_one ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_ice, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_ice, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_ice, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_ice, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cot_ice, vmax, 320.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cer, long, cer_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer, sname, cer_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer, units, unit_cer ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer, vmax, 200.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cer_liq, long, cer_liq_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_liq, units, unit_cer ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_liq, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_liq, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_liq, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_liq, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_liq, vmax, 200.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cer_ice, long, cer_ice_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_ice, units, unit_cer ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_ice, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_ice, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_ice, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_ice, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cer_ice, vmax, 200.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cwp, long, cwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp, vmax, 32000.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % lwp, long, lwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp, sname, lwp_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp, vmax, 32000.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % iwp, long, iwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp, sname, iwp_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp, vmax, 32000.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % mlwp, long, mlwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % mlwp, sname, mlwp_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % mlwp, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % mlwp, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % mlwp, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % mlwp, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % mlwp, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % mlwp, vmax, 32000.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % miwp, long, miwp_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % miwp, sname, miwp_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % miwp, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % miwp, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % miwp, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % miwp, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % miwp, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % miwp, vmax, 32000.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % cwp_allsky, long, cwp_allsky_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp_allsky, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp_allsky, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp_allsky, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp_allsky, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp_allsky, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % cwp_allsky, vmax, 32000.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, long, lwp_allsky_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, sname, lwp_allsky_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % lwp_allsky, vmax, 32000.0 ) )

        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, long, iwp_allsky_str ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, sname, iwp_allsky_std ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, units, unit_cwp ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, fill, sreal_fill_value ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, fscale, 1.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, offset, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, vmin, 0.0 ) )
        CALL CHECK ( nf90_put_att( fid, vids % iwp_allsky, vmax, 32000.0 ) )

    END SUBROUTINE DEFINE_ATTRIBUTES

    !==========================================================================

    SUBROUTINE CREATE_NC_FILENAME( pwd, sdate, thv, nc_file )

        IMPLICIT NONE

        REAL(KIND=sreal),           INTENT(IN)  :: thv
        CHARACTER(LEN=6),           INTENT(IN)  :: sdate
        CHARACTER(LEN=path_length), INTENT(IN)  :: pwd
        CHARACTER(LEN=path_length), INTENT(OUT) :: nc_file

        ! local variables
        CHARACTER(LEN=20) :: thv_int

        WRITE(thv_int, '(F4.2)') thv

        ! create output nc-filename
        nc_file = TRIM( pwd ) // "/ERA_Interim_MM" // TRIM(sdate) // &
                  '_cot-thv-' // TRIM(thv_int) // '.nc' 

    END SUBROUTINE CREATE_NC_FILENAME

    !==========================================================================

    SUBROUTINE DEFINE_GLOBALS( fid, set, aux, nfiles )

        USE FUNCS, only: timestamp
        IMPLICIT NONE

        TYPE(era_aux),      INTENT(IN) :: aux
        TYPE(config),       INTENT(IN) :: set
        INTEGER(KIND=lint), INTENT(IN) :: fid
        INTEGER(KIND=sint), INTENT(IN) :: nfiles

        ! local 
        CHARACTER(LEN=64) :: scops_def, mpc_def, ts
        CHARACTER(LEN=64) :: overlap_def
        CHARACTER(LEN=84) :: cwc_mod_def

        ts = timestamp()

        IF ( set % scops == dwd_scops )  scops_def = "DWD SCOPS"
        IF ( set % scops == cosp_scops ) scops_def = "COSP SCOPS"

        IF ( set % overlap == over_max )      overlap_def = "max"
        IF ( set % overlap == over_rand )     overlap_def = "random"
        IF ( set % overlap == over_max_rand ) overlap_def = "max/random"

        IF ( set % mpc == no_mixed_phase ) mpc_def = "separated phase"
        IF ( set % mpc == mixed_phase )    mpc_def = "mixed phase"

        IF ( set % cwc_mod == cwc_mod_off ) &
            cwc_mod_def = "original model CWC used"
        IF ( set % cwc_mod == cwc_mod_on )  &
            cwc_mod_def = "model CWC modified using t-profile (binning approach)"

        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, PROJECT, PROJECT_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, TITLE, TITLE_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, INS, DWD ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, VOC, VOC_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, SRC, SRC_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, TCS, set % start_date ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, TCE, set % end_date ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, TCR, TCR_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, THV, set % thv ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, NOF, nfiles ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, SCO, scops_def ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, OVE, overlap_def ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, MPC, mpc_def ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, CWCMOD, cwc_mod_def ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, RES, RES_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, LONRES, RES_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, LATRES, RES_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, LONUNI, LONUNI_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, LATUNI, LATUNI_STR ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, XMIN, MINVAL( aux % lon ) ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, XMAX, MAXVAL( aux % lon ) ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, YMIN, MINVAL( aux % lat ) ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, YMAX, MAXVAL( aux % lat ) ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, DTC, TRIM(ts) ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, CRU, DWD_URL ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, CRM, DWD_MAIL ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, CRE, DWD ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, CDMGRD, GRID ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, REFERENCES, ESA_URL ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, SUMMARY, SUMMARY_TXT ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, KEYWORDS, KEYWORDS_TXT ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, COMMENT, COMMENT_TXT ) )
        CALL CHECK ( nf90_put_att( fid, NF90_GLOBAL, LICENSE, LICENSE_TXT ) )

     END SUBROUTINE DEFINE_GLOBALS

    !==========================================================================

    SUBROUTINE DEFINE_COORD_VARS( fid, dim_lon, dim_lat, lon_id, lat_id, &
                                  phase_id, time_id , lon_dimid, lat_dimid, &
                                  phase_dimid, time_dimid )

        IMPLICIT NONE

        INTEGER(KIND=lint), INTENT(IN)  :: fid
        INTEGER(KIND=lint), INTENT(IN)  :: dim_lon, dim_lat
        INTEGER(KIND=lint), INTENT(OUT) :: lat_id, lon_id
        INTEGER(KIND=lint), INTENT(OUT) :: time_id, phase_id
        INTEGER(KIND=lint), INTENT(OUT) :: lat_dimid, lon_dimid
        INTEGER(KIND=lint), INTENT(OUT) :: time_dimid, phase_dimid
        ! local
        INTEGER(KIND=lint) :: dim_time, dim_phase

        dim_time = 1
        dim_phase = n_hist_phase

        ! define dimensions
        CALL CHECK ( nf90_def_dim( fid, "time", dim_time, time_dimid ) )
        CALL CHECK ( nf90_def_dim( fid, "lon",  dim_lon, lon_dimid ) )
        CALL CHECK ( nf90_def_dim( fid, "lat",  dim_lat, lat_dimid ) )

        ! define coordinate variables
        CALL CHECK ( nf90_def_var( fid, "time", NF90_DOUBLE, time_dimid, time_id ) )
        CALL CHECK ( nf90_def_var( fid, "lon",  NF90_FLOAT,  lon_dimid, lon_id ) )
        CALL CHECK ( nf90_def_var( fid, "lat",  NF90_FLOAT,  lat_dimid, lat_id ) )

        ! assign units attributes
        CALL CHECK ( nf90_put_att( fid, lon_id, units, LONUNI_STR ) )
        CALL CHECK ( nf90_put_att( fid, lat_id, units, LATUNI_STR ) )

        ! long_names
        CALL CHECK ( nf90_put_att( fid, lon_id, long, "longitude" ) )
        CALL CHECK ( nf90_put_att( fid, lat_id, long, "latitude" ) )

        CALL DEFINE_VAR( fid, 4, "hist_phase", dim_phase, phase_dimid, phase_id )

    END SUBROUTINE DEFINE_COORD_VARS

    !==========================================================================

    SUBROUTINE DEFINE_HIST1D ( fid, x_dimid, y_dimid, p_dimid, t_dimid, ids )

        IMPLICIT NONE

        INTEGER(KIND=lint), INTENT(IN)  :: fid
        INTEGER(KIND=lint), INTENT(IN)  :: x_dimid, y_dimid, p_dimid, t_dimid
        TYPE(hist1d_ids),   INTENT(OUT) :: ids

        ! local
        INTEGER(KIND=lint), DIMENSION(5) :: cot_hist_dims
        INTEGER(KIND=lint), DIMENSION(5) :: ctt_hist_dims, ctp_hist_dims
        INTEGER(KIND=lint), DIMENSION(5) :: cer_hist_dims, cwp_hist_dims
        INTEGER(KIND=lint) :: dim_cot_axis, dim_cot_bins
        INTEGER(KIND=lint) :: dim_ctp_axis, dim_ctp_bins
        INTEGER(KIND=lint) :: dim_ctt_axis, dim_ctt_bins
        INTEGER(KIND=lint) :: dim_cer_axis, dim_cer_bins
        INTEGER(KIND=lint) :: dim_cwp_axis, dim_cwp_bins
        INTEGER(KIND=lint) :: cot_axis_dimid, cot_bins_dimid
        INTEGER(KIND=lint) :: ctp_axis_dimid, ctp_bins_dimid
        INTEGER(KIND=lint) :: ctt_axis_dimid, ctt_bins_dimid
        INTEGER(KIND=lint) :: cer_axis_dimid, cer_bins_dimid
        INTEGER(KIND=lint) :: cwp_axis_dimid, cwp_bins_dimid

        dim_cot_axis = n_cot_bins + 1
        dim_cot_bins = n_cot_bins

        CALL DEFINE_VAR( fid, 2, "hist1d_cot_bin_border", &
                         dim_cot_axis, cot_axis_dimid, ids % cot_axis )
        CALL DEFINE_VAR( fid, 2, "hist1d_cot_bin_centre", &
                         dim_cot_bins, cot_bins_dimid, ids % cot_bins )

        cot_hist_dims = (/ x_dimid,y_dimid,cot_bins_dimid,p_dimid,t_dimid /)

        CALL DEFINE_OUT( fid, 1, "hist1d_cot", cot_hist_dims, ids % cot_hist )

        dim_ctp_axis = n_ctp_bins + 1
        dim_ctp_bins = n_ctp_bins

        CALL DEFINE_VAR( fid, 2, "hist1d_ctp_bin_border", &
                         dim_ctp_axis, ctp_axis_dimid, ids % ctp_axis )
        CALL DEFINE_VAR( fid, 2, "hist1d_ctp_bin_centre", &
                         dim_ctp_bins, ctp_bins_dimid, ids % ctp_bins )
        
        ctp_hist_dims = (/ x_dimid, y_dimid, ctp_bins_dimid, p_dimid, t_dimid /) 

        CALL DEFINE_OUT( fid, 1, "hist1d_ctp", ctp_hist_dims, ids % ctp_hist )

        dim_ctt_axis = n_ctt_bins + 1
        dim_ctt_bins = n_ctt_bins

        CALL DEFINE_VAR( fid, 2, "hist1d_ctt_bin_border", &
                         dim_ctt_axis, ctt_axis_dimid, ids % ctt_axis )
        CALL DEFINE_VAR( fid, 2, "hist1d_ctt_bin_centre", &
                         dim_ctt_bins, ctt_bins_dimid, ids % ctt_bins )
        
        ctt_hist_dims = (/ x_dimid, y_dimid, ctt_bins_dimid, p_dimid, t_dimid /) 

        CALL DEFINE_OUT( fid, 1, "hist1d_ctt", ctt_hist_dims, ids % ctt_hist )

        dim_cer_axis = n_cer_bins + 1
        dim_cer_bins = n_cer_bins

        CALL DEFINE_VAR( fid, 2, "hist1d_cer_bin_border", &
                         dim_cer_axis, cer_axis_dimid, ids % cer_axis )
        CALL DEFINE_VAR( fid, 2, "hist1d_cer_bin_centre", &
                         dim_cer_bins, cer_bins_dimid, ids % cer_bins )
        
        cer_hist_dims = (/ x_dimid, y_dimid, cer_bins_dimid, p_dimid, t_dimid /) 

        CALL DEFINE_OUT( fid, 1, "hist1d_cer", cer_hist_dims, ids % cer_hist )


        dim_cwp_axis = n_cwp_bins + 1
        dim_cwp_bins = n_cwp_bins

        CALL DEFINE_VAR( fid, 2, "hist1d_cwp_bin_border", &
                         dim_cwp_axis, cwp_axis_dimid, ids % cwp_axis )
        CALL DEFINE_VAR( fid, 2, "hist1d_cwp_bin_centre", &
                         dim_cwp_bins, cwp_bins_dimid, ids % cwp_bins )
        
        cwp_hist_dims = (/ x_dimid, y_dimid, cwp_bins_dimid, p_dimid, t_dimid /) 

        CALL DEFINE_OUT( fid, 1, "hist1d_cwp", cwp_hist_dims, ids % cwp_hist )

    END SUBROUTINE DEFINE_HIST1D

    !==========================================================================

    SUBROUTINE DEFINE_HIST2D ( fid, x_dimid, y_dimid, p_dimid, t_dimid, ids )

        IMPLICIT NONE

        INTEGER(KIND=lint), INTENT(IN)  :: fid
        INTEGER(KIND=lint), INTENT(IN)  :: x_dimid, y_dimid, p_dimid, t_dimid
        TYPE(hist2d_ids),   INTENT(OUT) :: ids

        ! local
        INTEGER(KIND=lint), DIMENSION(6) :: hist_dims
        INTEGER(KIND=lint)               :: dim_cot_axis, dim_cot_bins
        INTEGER(KIND=lint)               :: dim_ctp_axis, dim_ctp_bins
        INTEGER(KIND=lint)               :: cot_axis_dimid, cot_bins_dimid
        INTEGER(KIND=lint)               :: ctp_axis_dimid, ctp_bins_dimid

        dim_cot_axis = n_hist_cot
        dim_cot_bins = n_hist_cot - 1
        dim_ctp_axis = n_hist_ctp
        dim_ctp_bins = n_hist_ctp - 1

        CALL DEFINE_VAR( fid, 2, "hist2d_cot_bin_border", &
                         dim_cot_axis, cot_axis_dimid, ids % cot_axis )
        CALL DEFINE_VAR( fid, 2, "hist2d_cot_bin_centre", &
                         dim_cot_bins, cot_bins_dimid, ids % cot_bins )
        CALL DEFINE_VAR( fid, 2, "hist2d_ctp_bin_border", &
                         dim_ctp_axis, ctp_axis_dimid, ids % ctp_axis )
        CALL DEFINE_VAR( fid, 2, "hist2d_ctp_bin_centre", &
                         dim_ctp_bins, ctp_bins_dimid, ids % ctp_bins )

        hist_dims = (/ x_dimid, y_dimid, cot_bins_dimid, &
                       ctp_bins_dimid, p_dimid, t_dimid /) 

        CALL DEFINE_OUT( fid, 1, "hist2d_cot_ctp", hist_dims, ids % hist )

    END SUBROUTINE DEFINE_HIST2D

    !==========================================================================

    SUBROUTINE DEFINE_MM ( fid, x_dimid, y_dimid, t_dimid, ids )

        IMPLICIT NONE

        INTEGER(KIND=lint), INTENT(IN)  :: fid
        INTEGER(KIND=lint), INTENT(IN)  :: x_dimid, y_dimid, t_dimid
        TYPE(mm_ids),       INTENT(OUT) :: ids

        ! local
        INTEGER(KIND=lint), DIMENSION(3) :: dimids

        dimids = (/ x_dimid, y_dimid, t_dimid/)

        CALL DEFINE_OUT( fid, 1, "nobs", dimids, ids % nobs )
        CALL DEFINE_OUT( fid, 1, "nobs_lwp", dimids, ids % nobs_lwp )
        CALL DEFINE_OUT( fid, 1, "nobs_iwp", dimids, ids % nobs_iwp )
        CALL DEFINE_OUT( fid, 1, "nobs_mlwp", dimids, ids % nobs_mlwp )
        CALL DEFINE_OUT( fid, 1, "nobs_miwp", dimids, ids % nobs_miwp )
        CALL DEFINE_OUT( fid, 2, "cfc", dimids, ids % cfc )
        CALL DEFINE_OUT( fid, 2, "cfc_high", dimids, ids % cfc_high )
        CALL DEFINE_OUT( fid, 2, "cfc_mid", dimids, ids % cfc_mid )
        CALL DEFINE_OUT( fid, 2, "cfc_low", dimids, ids % cfc_low )
        CALL DEFINE_OUT( fid, 2, "cph", dimids, ids % cph )
        CALL DEFINE_OUT( fid, 2, "cph_day", dimids, ids % cph_day )
        CALL DEFINE_OUT( fid, 2, "ctp", dimids, ids % ctp )
        CALL DEFINE_OUT( fid, 2, "cth", dimids, ids % cth )
        CALL DEFINE_OUT( fid, 2, "ctt", dimids, ids % ctt )
        CALL DEFINE_OUT( fid, 2, "cot", dimids, ids % cot )
        CALL DEFINE_OUT( fid, 2, "cot_liq", dimids, ids % cot_liq )
        CALL DEFINE_OUT( fid, 2, "cot_ice", dimids, ids % cot_ice )
        CALL DEFINE_OUT( fid, 2, "cer", dimids, ids % cer )
        CALL DEFINE_OUT( fid, 2, "cer_liq", dimids, ids % cer_liq )
        CALL DEFINE_OUT( fid, 2, "cer_ice", dimids, ids % cer_ice )
        CALL DEFINE_OUT( fid, 2, "cwp", dimids, ids % cwp )
        CALL DEFINE_OUT( fid, 2, "lwp", dimids, ids % lwp )
        CALL DEFINE_OUT( fid, 2, "iwp", dimids, ids % iwp )
        CALL DEFINE_OUT( fid, 2, "mlwp", dimids, ids % mlwp )
        CALL DEFINE_OUT( fid, 2, "miwp", dimids, ids % miwp )
        CALL DEFINE_OUT( fid, 2, "cwp_allsky", dimids, ids % cwp_allsky )
        CALL DEFINE_OUT( fid, 2, "lwp_allsky", dimids, ids % lwp_allsky )
        CALL DEFINE_OUT( fid, 2, "iwp_allsky", dimids, ids % iwp_allsky )

    END SUBROUTINE DEFINE_MM

    !==========================================================================
    
    SUBROUTINE WRITE_HIST1D ( fid, xdim, ydim, n_bins, varid, var)

        IMPLICIT NONE

        INTEGER(KIND=lint), INTENT(IN) :: fid, varid
        INTEGER(KIND=lint), INTENT(IN) :: xdim, ydim, n_bins
        INTEGER(KIND=lint), INTENT(IN) :: var(:,:,:,:)

        ! local 
        INTEGER(KIND=lint), DIMENSION(5) :: start, counter, stride

        start = 1
        stride = 1
        counter(1) = xdim
        counter(2) = ydim
        counter(3) = n_bins
        counter(4) = n_hist_phase
        counter(5) = 1

        CALL CHECK( nf90_put_var(fid, varid, var, start, counter, stride) )

     END SUBROUTINE WRITE_HIST1D

    !==========================================================================
    
    SUBROUTINE WRITE_HIST2D ( fid, xdim, ydim, varid, var)

        IMPLICIT NONE

        INTEGER(KIND=lint), INTENT(IN) :: fid, xdim, ydim, varid
        INTEGER(KIND=lint), INTENT(IN) :: var(:,:,:,:,:)

        ! local 
        INTEGER(KIND=lint), DIMENSION(6) :: start, counter, stride

        start = 1
        stride = 1
        counter(1) = xdim
        counter(2) = ydim
        counter(3) = n_hist_cot - 1
        counter(4) = n_hist_ctp - 1
        counter(5) = n_hist_phase
        counter(6) = 1

        CALL CHECK( nf90_put_var(fid, varid, var, start, counter, stride) )

     END SUBROUTINE WRITE_HIST2D
     
    !==========================================================================

    SUBROUTINE WRITE_MONTHLY_MEAN( aux, set, fin, cnt )

        USE CALENDER

        IMPLICIT NONE

        TYPE(era_aux), INTENT(IN) :: aux
        TYPE(config),  INTENT(IN) :: set
        TYPE(l3_vars), INTENT(IN) :: fin
        TYPE(npoints), INTENT(IN) :: cnt

        ! local variables
        INTEGER(KIND=lint)         :: ncid
        CHARACTER(LEN=path_length) :: nc_file
        DOUBLE PRECISION           :: julday, ref_julday, itime
        TYPE(hist1d_ids)           :: h1d_ids
        TYPE(hist2d_ids)           :: h2d_ids
        TYPE(mm_ids)               :: var_ids

        ! dimensions
        INTEGER(KIND=lint)            :: DIM_LAT, DIM_LON
        INTEGER(KIND=sint), PARAMETER :: ref_year = 1970
        INTEGER(KIND=sint), PARAMETER :: ref_month = 1
        INTEGER(KIND=sint), PARAMETER :: ref_day = 1

        ! id's
        INTEGER(KIND=lint) :: n_bins
        INTEGER(KIND=lint) :: time_id, time_dimid
        INTEGER(KIND=lint) :: lon_id, lon_dimid
        INTEGER(KIND=lint) :: lat_id, lat_dimid
        INTEGER(KIND=lint) :: phase_id, phase_dimid

        PRINT*, "** WRITE_MONTHLY_MEAN"

        ! dimensions
        DIM_LON   = aux % nlon
        DIM_LAT   = aux % nlat

        ! time dimension
        CALL GREG2JD ( ref_year, ref_month, ref_day, ref_julday )
        CALL GREG2JD ( set % sy, set % sm, set % sd, julday )
        itime = julday - ref_julday

        ! create output nc-filename
        CALL CREATE_NC_FILENAME( set % out_path, set % start_date(1:6), &
                                 set % thv, nc_file )

        ! create ncdf file
        CALL CHECK ( nf90_create( TRIM(nc_file), nf90_clobber, ncid ) )

        ! global attributes
        CALL DEFINE_GLOBALS( ncid, set, aux, cnt % file_counter )

        ! define phase, time, lon, lat
        CALL DEFINE_COORD_VARS( ncid, DIM_LON, DIM_LAT, &
                                lon_id, lat_id, phase_id, time_id, &
                                lon_dimid, lat_dimid, phase_dimid, time_dimid )

        ! 2d histogram definitions
        CALL DEFINE_HIST2D ( ncid, lon_dimid, lat_dimid, &
                             phase_dimid, time_dimid, h2d_ids )

        ! 1d histogram definitions
        CALL DEFINE_HIST1D ( ncid, lon_dimid, lat_dimid, &
                             phase_dimid, time_dimid, h1d_ids )

        ! define monthly mean variables
        CALL DEFINE_MM ( ncid, lon_dimid, lat_dimid, time_id, var_ids )

        ! define attributes
        CALL DEFINE_ATTRIBUTES( ncid, phase_id, h2d_ids, h1d_ids, var_ids )

        ! end define mode
        CALL CHECK ( nf90_enddef(ncid) )


        ! write coordinate variables
        CALL CHECK ( nf90_put_var(ncid, time_id, itime) )
        CALL CHECK ( nf90_put_var(ncid, lon_id, aux % lon) )
        CALL CHECK ( nf90_put_var(ncid, lat_id, aux % lat) )
        CALL CHECK ( nf90_put_var(ncid, phase_id, set % hist_phase) )

        ! write 2d histogram
        CALL CHECK ( nf90_put_var(ncid, h2d_ids % cot_axis, set % hist_cot_2d_axis) )
        CALL CHECK ( nf90_put_var(ncid, h2d_ids % cot_bins, set % hist_cot_2d_bin) )
        CALL CHECK ( nf90_put_var(ncid, h2d_ids % ctp_axis, set % hist_ctp_2d_axis) )
        CALL CHECK ( nf90_put_var(ncid, h2d_ids % ctp_bins, set % hist_ctp_2d_bin) )

        CALL WRITE_HIST2D ( ncid, DIM_LON, DIM_LAT, &
                            h2d_ids % hist, fin % hist_cot_ctp )

        ! write 1d histograms
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % cot_axis, set % hist_cot_1d_axis) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % cot_bins, set % hist_cot_1d_bin) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % ctp_axis, set % hist_ctp_1d_axis) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % ctp_bins, set % hist_ctp_1d_bin) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % ctt_axis, set % hist_ctt_1d_axis) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % ctt_bins, set % hist_ctt_1d_bin) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % cer_axis, set % hist_cer_1d_axis) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % cer_bins, set % hist_cer_1d_bin) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % cwp_axis, set % hist_cwp_1d_axis) )
        CALL CHECK ( nf90_put_var(ncid, h1d_ids % cwp_bins, set % hist_cwp_1d_bin) )

        n_bins = n_cot_bins
        CALL WRITE_HIST1D ( ncid, DIM_LON, DIM_LAT, n_bins, &
                            h1d_ids % cot_hist, fin % hist_cot )
        n_bins = n_ctp_bins
        CALL WRITE_HIST1D ( ncid, DIM_LON, DIM_LAT, n_bins, &
                            h1d_ids % ctp_hist, fin % hist_ctp )
        n_bins = n_ctt_bins
        CALL WRITE_HIST1D ( ncid, DIM_LON, DIM_LAT, n_bins, &
                            h1d_ids % ctt_hist, fin % hist_ctt )
        n_bins = n_cer_bins
        CALL WRITE_HIST1D ( ncid, DIM_LON, DIM_LAT, n_bins, &
                            h1d_ids % cer_hist, fin % hist_cer )
        n_bins = n_cwp_bins
        CALL WRITE_HIST1D ( ncid, DIM_LON, DIM_LAT, n_bins, &
                            h1d_ids % cwp_hist, fin % hist_cwp )

        ! write netcdf monthly mean variables
        CALL CHECK ( nf90_put_var(ncid, var_ids % cfc, fin % cfc) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cfc_high, fin % cfc_high) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cfc_mid, fin % cfc_mid) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cfc_low, fin % cfc_low) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cph, fin % cph) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cph_day, fin % cph_day) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % ctp, fin % ctp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % ctt, fin % ctt) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cth, fin % cth) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cot, fin % cot) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cot_liq, fin % cot_liq) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cot_ice, fin % cot_ice) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cer, fin % cer) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cer_liq, fin % cer_liq) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cer_ice, fin % cer_ice) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cwp, fin % cwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % lwp, fin % lwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % iwp, fin % iwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % mlwp, fin % mlwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % miwp, fin % miwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % cwp_allsky, fin % cwp_allsky) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % lwp_allsky, fin % lwp_allsky) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % iwp_allsky, fin % iwp_allsky) )

        CALL CHECK ( nf90_put_var(ncid, var_ids % nobs, cnt % ctp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % nobs_lwp, cnt % lwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % nobs_iwp, cnt % iwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % nobs_mlwp, cnt % mlwp) )
        CALL CHECK ( nf90_put_var(ncid, var_ids % nobs_miwp, cnt % miwp) )

        ! close ncdf file
        CALL CHECK( nf90_close(  ncid ) )

        PRINT*, "   nc-file: ", TRIM(nc_file), " done "

    END SUBROUTINE WRITE_MONTHLY_MEAN

    !==========================================================================

    SUBROUTINE READ_AUX_DATA( sfile, sdata )

        IMPLICIT NONE

        INTEGER            :: ncid, DimID, VarID, i 
        CHARACTER(LEN=50)  :: VarName
        REAL(KIND=sreal)   :: scale_factor, add_offset
        INTEGER(KIND=lint) :: fill_value, missing_value
        CHARACTER(LEN=file_length), INTENT(IN) :: sfile
        TYPE(era_aux), INTENT(INOUT)           :: sdata

        PRINT*, "** READ_AUX_DATA"

        PRINT*, "   Read SST_FILE "//TRIM(sfile)

        ! open ncdf file
        CALL CHECK( nf90_open( sfile, nf90_nowrite, ncid) )

        ! longitude
        CALL CHECK( nf90_inq_dimid( ncid, 'longitude', DimID ) )
        CALL CHECK( nf90_inquire_dimension( ncid, DimID, VarName, sdata % nlon ) )
        ALLOCATE( sdata % lon( sdata % nlon ) )
        CALL CHECK( nf90_inq_varid( ncid, 'longitude', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata % lon ) )

        ! latitude
        CALL CHECK( nf90_inq_dimid( ncid, 'latitude', DimID ) )
        CALL CHECK( nf90_inquire_dimension( ncid, DimID, VarName, sdata % nlat ) )
        ALLOCATE( sdata % lat( sdata % nlat ) )
        CALL CHECK( nf90_inq_varid( ncid, 'latitude', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata % lat ) )

        ! sea surface temperature
        CALL CHECK( nf90_inq_varid( ncid, 'sst', VarID ) )
        ALLOCATE( sdata % sst2d( sdata % nlon, sdata % nlat ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata % sst2d ) )

        ! attributes
        CALL CHECK( nf90_get_att( ncid, VarID, 'scale_factor', scale_factor )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'add_offset', add_offset )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'missing_value', missing_value )  )
        CALL CHECK( nf90_get_att( ncid, VarID, '_FillValue', fill_value )  )
        
        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )


        PRINT*, "   Create Land/Sea mask: land = ",land," sea = ",sea 

        ALLOCATE( sdata % lsm2d( sdata % nlon, sdata % nlat ) )

        WHERE( sdata % sst2d == fill_value .OR. sdata % sst2d == missing_value ) 
            sdata % lsm2d = land
            sdata % sst2d = sint_fill_value
        ELSEWHERE
            sdata % lsm2d = sea
            sdata % sst2d = sdata % sst2d*scale_factor+add_offset
        END WHERE


        PRINT*, "   Create ERA-Interim 2D-grid" 

        ALLOCATE( sdata % lon2d( sdata % nlon, sdata % nlat ) )
        ALLOCATE( sdata % lat2d( sdata % nlon, sdata % nlat ) )
        
        ! each column of i-th row = longitude
        DO i=1, sdata % nlon
            sdata % lon2d(i,:) = sdata % lon(i)
        END DO
        ! each row of i-th column = latitude
        DO i=1, sdata % nlat
            sdata % lat2d(:,i) = sdata % lat(i)
        END DO

    END SUBROUTINE READ_AUX_DATA

    !==========================================================================

    SUBROUTINE READ_ERA_NCFILE( ifile, idata, settings )

        USE SUBS

        IMPLICIT NONE

        INTEGER(KIND=sint), PARAMETER :: fb=15
        CHARACTER(LEN=fb),  PARAMETER :: filbase="ERA_Interim_an_"
        INTEGER                       :: ncid, DimID, VarID, idx
        CHARACTER(LEN=50)             :: VarName
        CHARACTER(LEN=20)             :: string
        CHARACTER(LEN=file_length), INTENT(IN) :: ifile
        TYPE(era_input), INTENT(INOUT)         :: idata
        TYPE(config),    INTENT(INOUT)         :: settings

        !local variables: total CWC, scaling factor
        INTEGER(KIND=sint)                              :: z
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: mst_cwc
        REAL(KIND=sreal), DIMENSION(:,:,:), ALLOCATABLE :: mst_sca

        PRINT*, "** READ_ERA_NCFILE"

        ! open ncdf file
        CALL CHECK( nf90_open( ifile, nf90_nowrite, ncid) )

        ! 1D longitude
        CALL CHECK( nf90_inq_dimid( ncid, 'lon', DimID ) )
        CALL CHECK( nf90_inquire_dimension( ncid, DimID, VarName, idata % xdim ) )
        ALLOCATE( idata % lon( idata % xdim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'lon', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lon ) )

        ! 1D latitude
        CALL CHECK( nf90_inq_dimid( ncid, 'lat', DimID ) )
        CALL CHECK( nf90_inquire_dimension( ncid, DimID, VarName, idata % ydim ) )
        ALLOCATE( idata % lat( idata % ydim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'lat', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lat ) )

        ! 1D model levels
        CALL CHECK( nf90_inq_dimid( ncid, 'lev_2', DimID ) )
        CALL CHECK( nf90_inquire_dimension( ncid, DimID, VarName, idata % zdim ) )
        ALLOCATE( idata % lev( idata % zdim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'lev_2', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lev ) )

        ! 2D geopotential [m^2/s^2]
        ALLOCATE( idata % geop2d( idata % xdim, idata % ydim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'Z', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % geop2d ) )

        ! 2D logarithm of surface pressure [ ]
        ALLOCATE( idata % lnsp2d( idata % xdim, idata % ydim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'LNSP', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lnsp2d ) )

        ! 3D cloud cover
        ALLOCATE( idata % cc_prof( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'CC', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % cc_prof ) )

        ! 3D liquid cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        ALLOCATE( idata % lwc_prof( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'CLWC', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lwc_prof ) )

        ! 3D ice cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        ALLOCATE( idata % iwc_prof( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'CIWC', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % iwc_prof ) )

        ! 3D temperature [K]
        ALLOCATE( idata % temp_prof( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'T', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % temp_prof ) )

        ! 3D specific humidity [kg kg**-1]
        ALLOCATE( idata % shum_prof( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_inq_varid( ncid, 'Q', VarID ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % shum_prof ) )

        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )

        ! allocate SZA2d array, which will be filled later
        ALLOCATE( idata % sza2d( idata % xdim, idata % ydim ) )

        ! negative values set to zero
        WHERE ( idata % cc_prof  .LT. 0.0 ) idata % cc_prof  = 0.0
        WHERE ( idata % lwc_prof .LT. 0.0 ) idata % lwc_prof = 0.0
        WHERE ( idata % iwc_prof .LT. 0.0 ) idata % iwc_prof = 0.0

        ! modify lwc and iwc regarding temperature
        IF ( settings % cwc_mod == cwc_mod_on ) THEN 

            ALLOCATE( mst_cwc( idata % xdim, idata % ydim, idata % zdim ) )
            ALLOCATE( mst_sca( idata % xdim, idata % ydim, idata % zdim ) )

            ! z = 60: lowermost model level
            ! z =  1: uppermost model level
            DO z=idata % zdim, 1, -1

                ! total cloud water content
                mst_cwc(:,:,z) = idata % lwc_prof(:,:,z) + idata % iwc_prof(:,:,z)

                ! reset liquid and ice water content w.r.t. temperature
                ! LIQUID
                WHERE( idata % temp_prof(:,:,z) .GE. 273.15 )
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z)
                    idata % iwc_prof(:,:,z) = 0.
                END WHERE
                ! ICE
                WHERE( idata % temp_prof(:,:,z) .LT. 240.0 )
                    idata % lwc_prof(:,:,z) = 0.
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z)
                END WHERE
                ! IN BETWEEN
                WHERE( idata % temp_prof(:,:,z) .GE. 240. .AND. idata % temp_prof(:,:,z) .LT.  245. )
                    mst_sca(:,:,z) = 0.109
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z) * mst_sca(:,:,z)
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z) * ( 1.0 - mst_sca(:,:,z) )
                END WHERE
                WHERE( idata % temp_prof(:,:,z) .GE. 245. .AND. idata % temp_prof(:,:,z) .LT.  250. )
                    mst_sca(:,:,z) = 0.256
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z) * mst_sca(:,:,z)
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z) * ( 1.0 - mst_sca(:,:,z) )
                END WHERE
                WHERE( idata % temp_prof(:,:,z) .GE. 250. .AND. idata % temp_prof(:,:,z) .LT.  255. )
                    mst_sca(:,:,z) = 0.314
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z) * mst_sca(:,:,z)
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z) * ( 1.0 - mst_sca(:,:,z) )
                END WHERE
                WHERE( idata % temp_prof(:,:,z) .GE. 255. .AND. idata % temp_prof(:,:,z) .LT.  260. )
                    mst_sca(:,:,z) = 0.378
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z) * mst_sca(:,:,z)
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z) * ( 1.0 - mst_sca(:,:,z) )
                END WHERE
                WHERE( idata % temp_prof(:,:,z) .GE. 260. .AND. idata % temp_prof(:,:,z) .LT.  265. )
                    mst_sca(:,:,z) = 0.493
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z) * mst_sca(:,:,z)
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z) * ( 1.0 - mst_sca(:,:,z) )
                END WHERE
                WHERE( idata % temp_prof(:,:,z) .GE. 265. .AND. idata % temp_prof(:,:,z) .LT.  270. )
                    mst_sca(:,:,z) = 0.700
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z) * mst_sca(:,:,z)
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z) * ( 1.0 - mst_sca(:,:,z) )
                END WHERE
                WHERE( idata % temp_prof(:,:,z) .GE. 270. .AND. idata % temp_prof(:,:,z) .LT.  275. )
                    mst_sca(:,:,z) = 0.947
                    idata % lwc_prof(:,:,z) = mst_cwc(:,:,z) * mst_sca(:,:,z)
                    idata % iwc_prof(:,:,z) = mst_cwc(:,:,z) * ( 1.0 - mst_sca(:,:,z) )
                END WHERE

            END DO

            IF ( ALLOCATED( mst_cwc ) ) DEALLOCATE ( mst_cwc )
            IF ( ALLOCATED( mst_sca ) ) DEALLOCATE ( mst_sca )

        ENDIF ! modify lwc and iwc regarding temperature

        ! compute now: z=1, 60
        ! * geopotential profile = idata % geop_prof
        ! * pressure at levels   = idata % pres_prof
        ! * pressure difference  = idata % dpres_prof, e.g.
        !   z=60: p@60-p@59
        !   z=1 : p@1 (or zero)?
        ALLOCATE( idata % geop_prof ( idata%xdim, idata%ydim, idata%zdim ) )
        ALLOCATE( idata % pres_prof ( idata%xdim, idata%ydim, idata%zdim ) )
        ALLOCATE( idata % dpres_prof( idata%xdim, idata%ydim, idata%zdim ) )
        CALL CALC_GEOP_PRES_PROFILES( idata )

        ! split filename ERA_Interim_an_20080701_0000+00_mlev.nc
        idx = INDEX( TRIM(ifile), filbase )

        idata % filename = TRIM( ifile(idx:LEN_TRIM(ifile)) )
        idata % dirname  = TRIM( ifile(1:idx-1) )
        idata % basename = TRIM( ifile(idx:SCAN(TRIM(ifile),'.')-1) )

        string = TRIM( ifile(idx+fb:idx+fb+3) )
        READ(string, '(I4)') idata % year

        string = TRIM( ifile(idx+fb+4:idx+fb+5) )
        READ(string, '(I2)') idata % month

        string = TRIM( ifile(idx+fb+6:idx+fb+7) )
        READ(string, '(I2)') idata % day

        string = TRIM( ifile(idx+fb+9:idx+fb+10) )
        READ(string, '(I2)') idata % hour

    END SUBROUTINE READ_ERA_NCFILE

    !==========================================================================

    SUBROUTINE CHECK( status )

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: status

        IF (status /= nf90_noerr) THEN
            PRINT *, TRIM(nf90_strerror(status))
            STOP "Stopped"
        END IF

    END SUBROUTINE CHECK

    !==========================================================================

END MODULE SIM_NCDF
