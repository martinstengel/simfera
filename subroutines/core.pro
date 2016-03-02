;------------------------------------------------------------------------------
; IN : DATA, GRID, SZA, CWP, COT, CER, THRESHOLD, HISTO, MEANS
; OUT: TMP, MEANS
; search bottom-up, where is a cloud using COT threshold value
;------------------------------------------------------------------------------
PRO CORE, inp, grd, sza, scops_type, cwp, cot, cer, thv, mpc, $
          histo, means, tmp, TEST=test
;------------------------------------------------------------------------------

    ; fill_value
    fillvalue = -999.
    sv_cnt = 0

    ; 2D arrays containing the upper-most cloud information
    cfc_tmp = FLTARR(grd.XDIM,grd.YDIM)
    ctp_tmp = FLTARR(grd.XDIM,grd.YDIM)
    cth_tmp = FLTARR(grd.XDIM,grd.YDIM)
    ctt_tmp = FLTARR(grd.XDIM,grd.YDIM)
    cph_tmp = FLTARR(grd.XDIM,grd.YDIM)
    ; only for SZA LT 75. (day)
    cph_tmp_day = FLTARR(grd.XDIM,grd.YDIM)
    cwp_tmp = FLTARR(grd.XDIM,grd.YDIM)
    lwp_tmp = FLTARR(grd.XDIM,grd.YDIM)
    iwp_tmp = FLTARR(grd.XDIM,grd.YDIM)
    cwp_tmp_allsky = FLTARR(grd.XDIM,grd.YDIM)
    lwp_tmp_allsky = FLTARR(grd.XDIM,grd.YDIM)
    iwp_tmp_allsky = FLTARR(grd.XDIM,grd.YDIM)
    cot_tmp = FLTARR(grd.XDIM,grd.YDIM)
    lcot_tmp = FLTARR(grd.XDIM,grd.YDIM)
    icot_tmp = FLTARR(grd.XDIM,grd.YDIM)
    cer_tmp = FLTARR(grd.XDIM,grd.YDIM)
    lcer_tmp = FLTARR(grd.XDIM,grd.YDIM)
    icer_tmp = FLTARR(grd.XDIM,grd.YDIM)



    ; loop over individual grid cells
    FOR xi=0, grd.xdim-1, 1 DO BEGIN ;longitude
        FOR yi=0, grd.ydim-1, 1 DO BEGIN ;latitude 

            ; flag: 0=night, 1=day 
            IF ( sza[xi,yi] GE 75. ) THEN flag=0 ELSE flag=1


            ; gridbox mean profile downscaling -> gridbox subcol profiles
            SCOPS, scops_type, mpc, flag, REFORM(inp.CC[xi,yi,*]), $ 
                REFORM(cot.LIQ[xi,yi,*]), REFORM(cot.ICE[xi,yi,*]), $ 
                REFORM(cer.LIQ[xi,yi,*]), REFORM(cer.ICE[xi,yi,*]), $ 
                REFORM(cwp.LIQ[xi,yi,*]), REFORM(cwp.ICE[xi,yi,*]), $ 
                matrix


            ; pseudo-retrieval, i.e search for upper-most cloud via cot thv
            array_ctp = FLTARR(matrix.NCOL)*0.-1.
            array_cth = FLTARR(matrix.NCOL)*0.-1.
            array_ctt = FLTARR(matrix.NCOL)*0.-1.
            array_cfc = FLTARR(matrix.NCOL)*0.
            array_cph = FLTARR(matrix.NCOL)*0.-1.

            IF ( flag EQ 1 ) THEN BEGIN 
                array_cot = FLTARR(matrix.NCOL)*0.-1.
                array_cwp = FLTARR(matrix.NCOL)*0.-1.
                array_cer = FLTARR(matrix.NCOL)*0.-1.
            ENDIF 

            wothin = WHERE( matrix.TCOT LT thv, n_wothin )
            IF (n_wothin GT 0) THEN BEGIN 
                matrix.CFC[wothin]=0
                matrix.CPH[wothin]=-1
                IF (flag EQ 1) THEN BEGIN ;day
                    matrix.COT[wothin]=0.
                    matrix.CER[wothin]=-1.  
                    matrix.CWP[wothin]=0.
                ENDIF
            ENDIF

            FOR icol=0, matrix.NCOL-1 DO BEGIN

               wo = WHERE( matrix.CFC[icol,*] GT 0.5, nwo )

               IF (nwo GT 0) THEN BEGIN 
                   array_ctp[icol] = inp.PLEVEL[wo[0]]/100.    
                   array_cth[icol] = inp.GEOP[xi,yi,wo[0]]/9.81    
                   array_ctt[icol] = inp.TEMP[xi,yi,wo[0]]   
                   array_cph[icol] = ROUND(matrix.CPH[icol,wo[0]])
                   array_cfc[icol] = 1

                   IF (flag EQ 1) THEN BEGIN ;day 
                       array_cot[icol] = TOTAL(matrix.COT[icol,*])
                       array_cwp[icol] = TOTAL(matrix.CWP[icol,*]) 
                       array_cer[icol] = matrix.CER[icol,wo[0]]

                       scale = WHERE( array_cot GT 100., nscale )
                       IF ( nscale GT 0) THEN BEGIN
                           max_cot = 100.
                           sfactor = FLTARR(matrix.NCOL)*0.+1.
                           sfactor = max_cot / array_cot[scale]
                           array_cot[scale] = array_cot[scale] * sfactor
                           array_cwp[scale] = array_cwp[scale] * sfactor
                       ENDIF
                   ENDIF ;end day

               ENDIF ;end day+night
            ENDFOR ;end of get vertical subcolumn values


            ;; make scops snapshots
            ;IF (KEYWORD_SET(test) AND (inp.HOUR EQ '00') AND (sv_cnt LT 10)) THEN BEGIN
            ;    IF (TOTAL(matrix.CFC) GT 70 AND (sza[xi,yi] GT 16 AND sza[xi,yi] LT 18) $
            ;        AND (inp.lon[xi] LT 179.)) THEN BEGIN
            ;        IF (sv_cnt EQ 1) OR (sv_cnt EQ 9) THEN BEGIN

            ;            ; plot upper-most subcol value
            ;            PLOT_CLOUD_ARRAYS, array_ctp, array_cth, array_ctt, $
            ;                array_cph, array_cer, array_cfc, array_cot, array_cwp, $
            ;                matrix.NCOL, inp, sv_cnt, sza, xi, yi, scops_type, mpc, thv

            ;            ; plot vertical subcolumns from SCOPS
            ;            MAKE_SCOPS_SNAPSHOTS, inp, grd, sza, xi, yi, $
            ;                ROTATE(matrix.CFC,7), 'CFC', scops_type, mpc, thv, sv_cnt, $
            ;                do_profile=REVERSE(matrix.CFC_PROF)
            ;            MAKE_SCOPS_SNAPSHOTS, inp, grd, sza, xi, yi, $
            ;                ROTATE(matrix.CWP,7), 'CWP', scops_type, mpc, thv, sv_cnt, $
            ;                do_profile=REVERSE(matrix.CWP_PROF)
            ;            MAKE_SCOPS_SNAPSHOTS, inp, grd, sza, xi, yi, $
            ;                ROTATE(matrix.COT,7), 'COT', scops_type, mpc, thv, sv_cnt, $
            ;                do_profile=REVERSE(matrix.COT_PROF)
            ;            MAKE_SCOPS_SNAPSHOTS, inp, grd, sza, xi, yi, $
            ;                ROTATE(matrix.CER,7), 'CER', scops_type, mpc, thv, sv_cnt
            ;            MAKE_SCOPS_SNAPSHOTS, inp, grd, sza, xi, yi, $
            ;                ROTATE(matrix.CPH,7), 'CPH', scops_type, mpc, thv, sv_cnt

            ;        ENDIF
            ;        sv_cnt++
            ;    ENDIF
            ;ENDIF


            ; compute summary statistics
            wocl     = WHERE( array_cfc GT 0.5,nwocl)
            wocl_ice = WHERE( array_cfc GT 0.5 AND array_cph LT 0.5, nwocl_ice )
            wocl_liq = WHERE( array_cfc GT 0.5 AND array_cph GT 0.5, nwocl_liq )

            cfc_tmp[xi,yi] = MEAN( array_cfc )

            IF ( nwocl gt 0 ) THEN BEGIN ;liquid & ice

                ctp_tmp[xi,yi] = MEAN( array_ctp[wocl] )
                cth_tmp[xi,yi] = MEAN( array_cth[wocl] )
                ctt_tmp[xi,yi] = MEAN( array_ctt[wocl] )
                cph_tmp[xi,yi] = MEAN( array_cph[wocl] ) 

                IF ( flag EQ 1 ) THEN BEGIN ;day all
                    cph_tmp_day[xi,yi] = cph_tmp[xi,yi]
                    cot_tmp[xi,yi] = MEAN( array_cot[wocl] )
                    cwp_tmp[xi,yi] = MEAN( array_cwp[wocl] )
                    cwp_tmp_allsky[xi,yi] = cwp_tmp[xi,yi]*nwocl/matrix.NCOL
                    cer_tmp[xi,yi] = MEAN( array_cer[wocl] )
                ENDIF ELSE BEGIN ;night
                    cph_tmp_day[xi,yi] = fillvalue
                    cot_tmp[xi,yi] = fillvalue
                    cwp_tmp[xi,yi] = fillvalue
                    cwp_tmp_allsky[xi,yi] = fillvalue
                    cer_tmp[xi,yi] = fillvalue
                ENDELSE

            ENDIF

            IF ( nwocl_liq GT 0 AND flag EQ 1 ) THEN BEGIN ;day liquid
                lcer_tmp[xi,yi] = MEAN( array_cer[wocl_liq] )
                lcot_tmp[xi,yi] = MEAN( array_cot[wocl_liq] )
                lwp_tmp[xi,yi] = MEAN( array_cwp[wocl_liq] ) 
                lwp_tmp_allsky[xi,yi] = lwp_tmp[xi,yi]*nwocl_liq/matrix.NCOL
            ENDIF ELSE BEGIN ;night
                lcer_tmp[xi,yi] = fillvalue 
                lcot_tmp[xi,yi] = fillvalue
                lwp_tmp[xi,yi] = fillvalue
                lwp_tmp_allsky[xi,yi] = fillvalue
            ENDELSE

            IF ( nwocl_ice GT 0 AND flag EQ 1 ) THEN BEGIN ;day ice
                icer_tmp[xi,yi] = MEAN( array_cer[wocl_ice] )
                icot_tmp[xi,yi] = MEAN( array_cot[wocl_ice] )
                iwp_tmp[xi,yi] = MEAN( array_cwp[wocl_ice] ) 
                iwp_tmp_allsky[xi,yi] = iwp_tmp[xi,yi]*nwocl_ice/matrix.NCOL 
            ENDIF ELSE BEGIN ;night
                icer_tmp[xi,yi] = fillvalue
                icot_tmp[xi,yi] = fillvalue
                iwp_tmp[xi,yi] = fillvalue
                iwp_tmp_allsky[xi,yi] = fillvalue
            ENDELSE


            ; collect HISTOS
            q = WHERE( array_cfc GT 0, n_q )
            IF ( n_q GT 0 ) THEN BEGIN 

                FOR opi=0, n_q-1 DO BEGIN 
                    
                    zug_cph=1-array_cph[q[opi]]

                    zug_ctp=(WHERE(histo.CTP1D-array_ctp[q[opi]] GT 0.))[0]-1
                    means.HIST1D_CTP[xi,yi,zug_ctp,zug_cph]++

                    zug_ctt=(WHERE(histo.CTT1D-array_ctt[q[opi]] GT 0.))[0]-1
                    means.HIST1D_CTT[xi,yi,zug_ctt,zug_cph]++

                    IF ( flag EQ 1 ) THEN BEGIN ;day
                        IF ( array_cot[q[opi]] GT 0. ) THEN BEGIN

                            zug_cot=(WHERE(histo.COT1D-array_cot[q[opi]] GT 0.))[0]-1
                            means.HIST1D_COT[xi,yi,zug_cot,zug_cph]++

                            zug_cer=(WHERE(histo.CER1D-array_cer[q[opi]] GT 0.))[0]-1
                            means.HIST1D_CER[xi,yi,zug_cer,zug_cph]++

                            zug_cwp=(WHERE(histo.CWP1D-(array_cwp[q[opi]]*1000.) GT 0.))[0]-1
                            means.HIST1D_CWP[xi,yi,zug_cwp,zug_cph]++

                            means.HIST2D_COT_CTP[xi,yi,zug_cot,zug_ctp,zug_cph]++

                        ENDIF
                    ENDIF ;end day

                ENDFOR

            ENDIF ; end of collect HISTOS

            UNDEFINE, array_ctp, array_cth, array_ctt, array_cfc
            UNDEFINE, array_cph, array_cot, array_cwp, array_cer

        ENDFOR ; latitude
    ENDFOR ; longitude
    ; loop over individual grid cells


    ; output structure
    tmp = {temp_arrays, $
           cfc:cfc_tmp, cph:cph_tmp, cph_day:cph_tmp_day, $
           ctt:ctt_tmp, cth:cth_tmp, ctp:ctp_tmp, $
           cwp:cwp_tmp, lwp:lwp_tmp, iwp:iwp_tmp, $
           cwp_allsky:cwp_tmp_allsky, $
           lwp_allsky:lwp_tmp_allsky, $
           iwp_allsky:iwp_tmp_allsky, $
           cot:cot_tmp, cot_liq:lcot_tmp, cot_ice:icot_tmp, $
           cer:cer_tmp, cer_liq:lcer_tmp, cer_ice:icer_tmp }

END
