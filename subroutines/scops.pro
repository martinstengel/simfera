;------------------------------------------------------------------------------
; IN : type, mpc, flag, icc, lcot, icot, lcer, icer, lwp, iwp
; OUT: subcols
; downscaling of grid cell into subcolumns
;------------------------------------------------------------------------------
PRO SCOPS, type, mpc, flag, icc, lcot, icot, lcer, icer, lwp, iwp, subcols

    cwp_prof_total = 0. > (lwp + iwp)
    cot_prof_total = 0. > (lcot + icot)
    cfc_lay_prof = icc[0:N_ELEMENTS(icc[*])-2]*0.5 $
                 + icc[1:N_ELEMENTS(icc[*])-1]*0.5

    nlev = N_ELEMENTS(cfc_lay_prof)
    ncol = 20

    matrix_tcot = FLTARR(ncol,nlev)*0.
    matrix_cot  = FLTARR(ncol,nlev)*0.
    matrix_cfc  = FLTARR(ncol,nlev)*0.
    matrix_cph  = FLTARR(ncol,nlev)*0.-1.

    IF (flag EQ 1) THEN BEGIN 
        matrix_cwp  = FLTARR(ncol,nlev)*0. 
        matrix_cer  = FLTARR(ncol,nlev)*0.-1.
    ENDIF ELSE BEGIN
        matrix_cwp  = 0. 
        matrix_cer  = 0.
    ENDELSE

    ;MST
    ;mpc = 0 ;no mixed phase clouds, only separate liquid and ice clouds
    ;mpc = 1 ;all clouds are mixed phase clouds if both water/ice contents exist

    first = -1
    lastcloud = -2

    FOR zi=0, nlev-1 DO BEGIN

        nfb = FLOOR(ncol * cfc_lay_prof[zi]) ;nfilledboxes

        IF (nfb GT 0) THEN BEGIN 

            nfb_liq = -1
            nfb_ice = -1 
            cwp_all = lwp[zi] + iwp[zi]
            cot_all = lcot[zi] + icot[zi]

            IF ( cwp_all GT 0. ) THEN BEGIN
                ;number of liquid clouds; only needed for mpc=0 option
                nfb_liq = FLOOR( nfb * (lwp[zi] / cwp_all) )
                ;number of ice clouds; only needed for mpc=0 option 
                nfb_ice = nfb - nfb_liq 
            ENDIF


            IF (lastcloud NE (zi-1) OR type EQ 1) THEN BEGIN
                x = LINDGEN(ncol)
                y = RANDOMU(dseed, ncol)
                ci = x[SORT(y)]
            ENDIF


            ; for no-mixed-phase-clouds option (mpc=0), just separate ones
            IF ( mpc EQ 0 ) THEN BEGIN

               IF ( nfb_ice GT 0 ) THEN BEGIN ;ice

                   IF (flag EQ 1) THEN BEGIN ;day
                       matrix_cwp[ci[0:nfb_ice-1],zi] = iwp[zi] 
                       matrix_cer[ci[0:nfb_ice-1],zi] = icer[zi]
                   ENDIF

                   matrix_cot[ci[0:nfb_ice-1],zi] = icot[zi] 
                   matrix_cfc[ci[0:nfb_ice-1],zi] = 1
                   matrix_cph[ci[0:nfb_ice-1],zi] = 0

               ENDIF

               IF (nfb_liq GT 0) THEN BEGIN ;liquid

                   IF (flag EQ 1) THEN BEGIN ;day
                       matrix_cwp[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = lwp[zi] 
                       matrix_cer[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = lcer[zi]
                   ENDIF

                   matrix_cot[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = lcot[zi]
                   matrix_cfc[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = 1
                   matrix_cph[ci[nfb_ice:nfb_ice+nfb_liq-1],zi] = 1

               ENDIF 

            ; for mixed-phase-clouds option (mpc=1)
            ENDIF ELSE BEGIN 

                matrix_cot[ci[0:nfb-1],zi] = cot_all 

                IF (flag EQ 1) THEN matrix_cwp[ci[0:nfb-1],zi] = cwp_all

                IF ( cwp_all GT 0. ) THEN BEGIN 

                    IF (flag EQ 1) THEN BEGIN ; day CER weighted mean
                        matrix_cer[ci[0:nfb-1],zi] = $
                            ( lcer[zi]*lwp[zi] + icer[zi]*iwp[zi] ) / cwp_all
                    ENDIF

                    ; liquid fraction
                    matrix_cph[ci[0:nfb-1],zi] = lwp[zi] / cwp_all
                ENDIF 

                matrix_cfc[ci[0:nfb-1],zi] = 1

           ENDELSE 

           IF (flag EQ 1) THEN matrix_cwp[ci[0:nfb-1],zi] = cwp_prof_total[zi]
           matrix_cot[ci[0:nfb-1],zi] = cot_prof_total[zi]
           matrix_cfc[ci[0:nfb-1],zi] = 1.0
           lastcloud = zi

        ENDIF

        IF (zi EQ 0) THEN matrix_tcot[*,zi] = matrix_cot[*,0]
        IF (zi GT 0) THEN matrix_tcot[*,zi] = TOTAL(matrix_cot[*,0:zi],2)

    ENDFOR

    subcols = { ncol:ncol, cfc_prof:cfc_lay_prof, $
                cot_prof:cot_prof_total, cwp_prof:cwp_prof_total, $
                tcot:matrix_tcot, cot:matrix_cot, cfc:matrix_cfc, $
                cph:matrix_cph, cwp:matrix_cwp, cer:matrix_cer }

    UNDEFINE, cfc_lay_prof, cot_prof_total, cwp_prof_total
    UNDEFINE, matrix_tcot, matrix_cot, matrix_cfc, $
              matrix_cph, matrix_cwp, matrix_cer

END
