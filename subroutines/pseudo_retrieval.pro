;------------------------------------------------------------------------------
; IN : DATA, GRID, SZA, CWP, COT, CER, THRESHOLD, HISTO, MEANS
; OUT: TMP, MEANS
; search bottom-up, where is a cloud using COT threshold value
;------------------------------------------------------------------------------
PRO PSEUDO_RETRIEVAL, inp, grd, sza, scops_type, cwp, cot, cer, thv, histo, $
                      means, tmp
;------------------------------------------------------------------------------

    ; fill_value
    fillvalue = -999.

    ; 2D arrays containing the upper-most cloud information
    ctp_tmp  = FLTARR(grd.XDIM,grd.YDIM) & ctp_tmp[*,*] = fillvalue
    cth_tmp  = FLTARR(grd.XDIM,grd.YDIM) & cth_tmp[*,*] = fillvalue
    ctt_tmp  = FLTARR(grd.XDIM,grd.YDIM) & ctt_tmp[*,*] = fillvalue
    cph_tmp  = FLTARR(grd.XDIM,grd.YDIM) & cph_tmp[*,*] = fillvalue
    cfc_tmp  = FLTARR(grd.XDIM,grd.YDIM) & cfc_tmp[*,*] = 0.
    cwp_tmp  = FLTARR(grd.XDIM,grd.YDIM) & cwp_tmp[*,*] = 0.
    lwp_tmp  = FLTARR(grd.XDIM,grd.YDIM) & lwp_tmp[*,*] = 0.
    iwp_tmp  = FLTARR(grd.XDIM,grd.YDIM) & iwp_tmp[*,*] = 0.
    cot_tmp  = FLTARR(grd.XDIM,grd.YDIM) & cot_tmp[*,*] = 0.
    lcot_tmp = FLTARR(grd.XDIM,grd.YDIM) & lcot_tmp[*,*] = 0.
    icot_tmp = FLTARR(grd.XDIM,grd.YDIM) & icot_tmp[*,*] = 0.
    cer_tmp  = FLTARR(grd.XDIM,grd.YDIM) & cer_tmp[*,*] = 0.
    lcer_tmp = FLTARR(grd.XDIM,grd.YDIM) & lcer_tmp[*,*] = 0.
    icer_tmp = FLTARR(grd.XDIM,grd.YDIM) & icer_tmp[*,*] = 0.


    ;MST
    ;mpc = 0 ;no mixed phase clouds, only separate liquid and ice clouds
    mpc = 1 ;all clouds are mixed phase clouds if both water/ice contents exist


    ; loop over individual grid cells
    FOR xi=0, grd.xdim-1, 1 DO BEGIN ;longitude
        FOR yi=0, grd.ydim-1, 1 DO BEGIN ;latitude 

            cot_prof_total = 0. > (REFORM(cot.LIQ[xi,yi,*]+cot.ICE[xi,yi,*]))
            cwp_prof_total = 0. > (REFORM(cwp.LIQ[xi,yi,*]+cwp.ICE[xi,yi,*]))
            cfc_lay_prof = inp.CC[xi,yi,0:N_ELEMENTS(inp.CC[0,0,*])-2]*0.5 + $
                           inp.CC[xi,yi,1:N_ELEMENTS(inp.CC[0,0,*])-1]*0.5
            cfc_lay_prof = 0. > REFORM(cfc_lay_prof[*])
            nlev = N_ELEMENTS(cfc_lay_prof)
            ncol = 20

            matrix_tcot = FLTARR(ncol,nlev)*0.
            matrix_cot  = FLTARR(ncol,nlev)*0.
            matrix_cwp  = FLTARR(ncol,nlev)*0.
            matrix_cer  = FLTARR(ncol,nlev)*0.-1.
            matrix_cfc  = FLTARR(ncol,nlev)*0.
            matrix_cph  = FLTARR(ncol,nlev)*0.-1.
            array_cot   = FLTARR(ncol)*0.-1.
            array_cwp   = FLTARR(ncol)*0.-1.
            array_cer   = FLTARR(ncol)*0.-1.
            array_ctp   = FLTARR(ncol)*0.-1.
            array_cth   = FLTARR(ncol)*0.-1.
            array_ctt   = FLTARR(ncol)*0.-1.
            array_cfc   = FLTARR(ncol)*0.
            array_cph   = FLTARR(ncol)*0.-1.



            first = -1
            lastcloud = -2

            FOR zi=0, nlev-1 DO BEGIN ; SCOPS ----------------------------------

                nfb = FLOOR(ncol * cfc_lay_prof[zi]) ;nfilledboxes

                IF (nfb GT 0) THEN BEGIN 

                    nfb_liq=-1
                    nfb_ice=-1 

                    IF ( (cwp.LIQ[xi,yi,zi] + cwp.ICE[xi,yi,zi]) GT 0. ) THEN BEGIN
                        ;number of liquid clouds; only needed for mpc=0 option
                        nfb_liq = FLOOR( nfb * (cwp.LIQ[xi,yi,zi] / (cwp.LIQ[xi,yi,zi] + cwp.ICE[xi,yi,zi]) ) )
                        ;number of ice clouds; only needed for mpc=0 option 
                        nfb_ice = nfb-nfb_liq 
                    ENDIF


                    IF (lastcloud NE (zi-1) OR scops_type EQ 1) THEN BEGIN
                        x = LINDGEN(ncol)
                        y = RANDOMU(dseed, ncol)
                        ci = x[SORT(y)]
                    ENDIF


                    ; for no-mixed-phase-clouds option (mpc=0), just separate ones
                    IF (mpc EQ 0) THEN BEGIN

                       IF (nfb_ice GT 0) THEN BEGIN 
                           matrix_cot[ci[0:nfb_ice-1],zi] = cot.ICE[xi,yi,zi] 
                           matrix_cwp[ci[0:nfb_ice-1],zi] = cot.ICE[xi,yi,zi]
                           matrix_cer[ci[0:nfb_ice-1],zi] = cer.ICE[xi,yi,zi]
                           matrix_cfc[ci[0:nfb_ice-1],zi] = 1
                           matrix_cph[ci[0:nfb_ice-1],zi] = 0
                       ENDIF
                       IF (nfb_liq GT 0) THEN BEGIN 
                           matrix_cot[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = cot.LIQ[xi,yi,zi]
                           matrix_cwp[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = cwp.LIQ[xi,yi,zi]
                           matrix_cer[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = cer.LIQ[xi,yi,zi]
                           matrix_cfc[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = 1
                           matrix_cph[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = 1
                       ENDIF 

                    ; for mixed-phase-clouds option (mpc=1)
                    ENDIF ELSE BEGIN 

                        matrix_cot[ci[0:nfb-1],zi] = cot.LIQ[xi,yi,zi] + cot.ICE[xi,yi,zi] 
                        matrix_cwp[ci[0:nfb-1],zi] = cwp.LIQ[xi,yi,zi] + cwp.ICE[xi,yi,zi]

                        IF ( (cwp.LIQ[xi,yi,zi] + cwp.ICE[xi,yi,zi]) GT 0. ) THEN BEGIN 
                            ; weighted mean 
                            matrix_cer[ci[0:nfb-1],zi] = (cer.LIQ[xi,yi,zi]*cwp.LIQ[xi,yi,zi] $ 
                                                        + cer.ICE[xi,yi,zi]*cwp.ICE[xi,yi,zi])$
                                                        / (cwp.LIQ[xi,yi,zi] + cwp.ICE[xi,yi,zi])
                            ; liquid fraction
                            matrix_cph[ci[0:nfb-1],zi] = cwp.LIQ[xi,yi,zi] / $
                                                        (cwp.LIQ[xi,yi,zi] + cwp.ICE[xi,yi,zi])
                        ENDIF 

                        matrix_cfc[ci[0:nfb-1],zi] = 1

                   ENDELSE 

                   matrix_cot[ci[0:nfb-1],zi] = cot_prof_total[zi]
                   matrix_cwp[ci[0:nfb-1],zi] = cwp_prof_total[zi]
                   matrix_cfc[ci[0:nfb-1],zi] = 1.0
                   lastcloud = zi

                ENDIF

                IF (zi EQ 0) THEN matrix_tcot[*,zi] = matrix_cot[*,0]
                IF (zi GT 0) THEN matrix_tcot[*,zi] = TOTAL(matrix_cot[*,0:zi],2)

            ENDFOR ; END SCOPS -------------------------------------------------
            


            wothin = WHERE( matrix_tcot lt thv, n_wothin )
            matrix_cot2 = matrix_cot

            IF (n_wothin GT 0) THEN BEGIN 
                matrix_cfc[wothin]=0
                matrix_cph[wothin]=-1
                matrix_cwp[wothin]=0.
                matrix_cot[wothin]=0.
                matrix_cer[wothin]=-1.
            ENDIF



            ; get subcolumn values
            FOR icol=0, ncol-1 DO BEGIN
               wo = WHERE( matrix_cfc[icol,*] GT 0.5, nwo )
               IF (nwo GT 0) THEN BEGIN 
                   array_ctp[icol]=inp.plevel[wo[0]]/100.    
                   array_cth[icol]=inp.GEOP[xi,yi,wo[0]]/9.81    
                   array_ctt[icol]=inp.TEMP[xi,yi,wo[0]]   
                   array_cph[icol]=ROUND(matrix_cph[icol,wo[0]])
                   array_cer[icol]=matrix_cer[icol,wo[0]]
                   array_cfc[icol]=1
                   array_cot[icol]=total(matrix_cot[icol,*])
                   array_cwp[icol]=total(matrix_cwp[icol,*])
               ENDIF
            ENDFOR


            ; SCALE cot & cwp
            scale = WHERE( array_cot GT 100., nscale )
            IF ( nscale GT 0) THEN BEGIN
                max_cot = 100.
                sfactor = FLTARR(ncol)*0.+1.
                sfactor = max_cot / array_cot[scale]
                array_cot[scale] = array_cot[scale] * sfactor
                array_cwp[scale] = array_cwp[scale] * sfactor
            ENDIF


            ; SOLAR cot & cwp & cer
            night = WHERE( sza GE 75., nnight, COMPLEMENT=day, NCOMPLEMENT=nday)
            IF ( nnight GT 0 ) THEN BEGIN
                array_cot[night] = 0.
                array_cwp[night] = 0.
                array_cer[night] = 0.
            ENDIF


            wocl     = WHERE( array_cfc GT 0.5,nwocl)
            wocl_ice = WHERE( array_cfc GT 0.5 AND array_cph LT 0.5, nwocl_ice )
            wocl_liq = WHERE( array_cfc GT 0.5 AND array_cph GT 0.5, nwocl_liq )

            cfc_tmp[xi,yi] = MEAN( array_cfc )

            IF (nwocl gt 0) THEN BEGIN 
                cot_tmp[xi,yi] = MEAN( array_cot[wocl] )
                cwp_tmp[xi,yi] = MEAN( array_cwp[wocl] )
                cer_tmp[xi,yi] = MEAN( array_cer[wocl] )
                ctp_tmp[xi,yi] = MEAN( array_ctp[wocl] )
                cth_tmp[xi,yi] = MEAN( array_cth[wocl] )
                ctt_tmp[xi,yi] = MEAN( array_ctt[wocl] )
                cph_tmp[xi,yi] = MEAN( array_cph[wocl] ) 
            ENDIF

            IF (nwocl_liq gt 0) THEN BEGIN 
                lcer_tmp[xi,yi] = MEAN( array_cer[wocl_liq] )
                lcot_tmp[xi,yi] = MEAN( array_cot[wocl_liq] )
                lwp_tmp[xi,yi] = MEAN( array_cwp[wocl_liq] ) 
            ENDIF

            IF (nwocl_ice gt 0) THEN BEGIN 
                icer_tmp[xi,yi] = MEAN( array_cer[wocl_ice] )
                icot_tmp[xi,yi] = MEAN( array_cot[wocl_ice] )
                iwp_tmp[xi,yi] = MEAN( array_cwp[wocl_ice] ) 
            ENDIF


            ; collect HIST1D --------------------------------------------------
            
            res = SUMUP_HIST1D(bin_dim=histo.CTP_BIN1D_DIM, $
                               cph_dim=histo.PHASE_DIM, $
                               lim_bin=histo.CTP2D, $
                               var_tmp=array_ctp, $
                               cfc_tmp=array_cfc, $
                               cph_tmp=array_cph )
            means.HIST1D_CTP[xi,yi,*,*] = means.HIST1D_CTP[xi,yi,*,*] + TOTAL(TOTAL(res,1),1)

            res = SUMUP_HIST1D(bin_dim=histo.CTT_BIN1D_DIM, $
                               cph_dim=histo.PHASE_DIM, $
                               lim_bin=histo.CTT2D, $
                               var_tmp=array_ctt, $
                               cfc_tmp=array_cfc, $
                               cph_tmp=array_cph )
            means.HIST1D_CTT[xi,yi,*,*] = means.HIST1D_CTT[xi,yi,*,*] + TOTAL(TOTAL(res,1),1)

            res = SUMUP_HIST1D(bin_dim=histo.COT_BIN1D_DIM, $
                               cph_dim=histo.PHASE_DIM, $
                               lim_bin=histo.COT2D, $
                               var_tmp=array_cot, $
                               cfc_tmp=array_cfc, $
                               cph_tmp=array_cph )
            means.HIST1D_COT[xi,yi,*,*] = means.HIST1D_COT[xi,yi,*,*] + TOTAL(TOTAL(res,1),1)

            res = SUMUP_HIST1D(bin_dim=histo.CER_BIN1D_DIM, $
                               cph_dim=histo.PHASE_DIM, $
                               lim_bin=histo.CER2D, $
                               var_tmp=array_cer, $
                               cfc_tmp=array_cfc, $
                               cph_tmp=array_cph )
            means.HIST1D_CER[xi,yi,*,*] = means.HIST1D_CER[xi,yi,*,*] + TOTAL(TOTAL(res,1),1)

            res = SUMUP_HIST1D(bin_dim=histo.CWP_BIN1D_DIM, $
                               cph_dim=histo.PHASE_DIM, $
                               lim_bin=histo.CWP2D, $
                               var_tmp=array_cwp*1000., $
                               cfc_tmp=array_cfc, $
                               cph_tmp=array_cph )
            means.HIST1D_CWP[xi,yi,*,*] = means.HIST1D_CWP[xi,yi,*,*] + TOTAL(TOTAL(res,1),1)


            ; collect HIST2D --------------------------------------------------

            res = SUMUP_HIST2D(histo, array_cot, array_ctp, array_cfc, array_cph)
            means.HIST2D_COT_CTP[xi,yi,*,*,*] = means.HIST2D_COT_CTP[xi,yi,*,*,*] + TOTAL(TOTAL(res,1),1)


        ENDFOR ; latitude
    ENDFOR ; longitude
    ; loop over individual grid cells


    ; output structure
    tmp = {temp_arrays, $
           cfc:cfc_tmp, cph:cph_tmp, $
           ctt:ctt_tmp, cth:cth_tmp, ctp:ctp_tmp, $
           cwp:cwp_tmp, lwp:lwp_tmp, iwp:iwp_tmp, $
           cot:cot_tmp, cot_liq:lcot_tmp, cot_ice:icot_tmp, $
           cer:cer_tmp, cer_liq:lcer_tmp, cer_ice:icer_tmp }

END
