;---------------------------------------------------------------
FUNCTION INIT_LSM_ARRAY, grd, sst, void, MAP=map 
;---------------------------------------------------------------

    lsm2d = INTARR(grd.XDIM,grd.YDIM)
    lsm2d[*,*] = -999

    land = WHERE( sst EQ MAX(sst[void]) )
    sea  = WHERE( SST GT MAX(sst[void]) )

    lsm2d[land] = 1
    lsm2d[sea] = 0

    IF KEYWORD_SET(map)  THEN BEGIN

        tit = 'Land/Sea Mask for ERA-Interim grid 0.5 x 0.5'
        fil = 'lsm_era_interim_0.5_0.5'

        PLOT_LSM2D, FILENAME=fil, DATA=lsm2d, TITLE=tit, $
                    LATITUDE=grd.lat2d, LONGITUDE=grd.lon2d

    ENDIF

    RETURN, lsm2d

END


;-----------------------------------------------------------------------------
FUNCTION INIT_SZA_ARRAY, data, grd, MAP=map 
;-----------------------------------------------------------------------------

    jul_day = DOY(data.YEAR, data.MONTH, data.DAY)

    utc2d = FLTARR(grd.XDIM,grd.YDIM) & utc2d[*,*] = data.HOUR
    day2d = FLTARR(grd.XDIM,grd.YDIM) & day2d[*,*] = jul_day

    ZENSUN, day2d, utc2d, grd.lat2d, grd.lon2d, sza2d

    IF KEYWORD_SET(map)  THEN BEGIN

        date  = data.YEAR + data.MONTH + data.DAY
        title ='SZA [deg] for ' + date + ' UTC  ' + data.UTC

        PLOT_SZA2D, FILENAME=data.FILENAME, DATA=sza2d, $
                    LATITUDE=grd.lat2d, LONGITUDE=grd.lon2d, $
                    TITLE=title

    ENDIF

    RETURN, sza2d

END


;-------------------------------------------------------------------
; IN  : DATA
; OUT : GRID
;-------------------------------------------------------------------
PRO INIT_ERA_GRID, data, grid
;-------------------------------------------------------------------

    ; set x,y,z dimensions using liquid water content variable
    xdim = FIX(N_ELEMENTS(data.lwc[*,0,0]))
    ydim = FIX(N_ELEMENTS(data.lwc[0,*,0]))
    zdim = FIX(N_ELEMENTS(data.lwc[0,0,*]))
    
    ; define longitude & latitude arrays ;[cols, rows]
    longrd=FLTARR(xdim,ydim)
    latgrd=FLTARR(xdim,ydim)
    
    ; create lat/lon grid arrays using lon & lat from ncfile
    FOR loi=0,xdim-1 DO longrd[loi,*]=data.lon[loi]
    FOR lai=0,ydim-1 DO latgrd[*,lai]=data.lat[lai]

    grid = { era_grid, $
             lon2d:longrd, lat2d:latgrd, $
             xdim:xdim, ydim:ydim, zdim:zdim }

END


;-------------------------------------------------------------------
; IN : GRIS, HIST_INFO
; OUT: ARRAYS, COUNTS
;-------------------------------------------------------------------
PRO INIT_OUT_ARRAYS, grid, hist, arrays, counts
;-------------------------------------------------------------------

    cfc = FLTARR(grid.XDIM,grid.YDIM) & cfc[*,*] = 0 ; cloud fraction
    cph = FLTARR(grid.XDIM,grid.YDIM) & cph[*,*] = 0 ; cloud phase
    cph_day = FLTARR(grid.XDIM,grid.YDIM) & cph_day[*,*] = 0 ; cloud phase day only
    ctt = FLTARR(grid.XDIM,grid.YDIM) & ctt[*,*] = 0 ; cloud top temperature
    cth = FLTARR(grid.XDIM,grid.YDIM) & cth[*,*] = 0 ; cloud top height
    ctp = FLTARR(grid.XDIM,grid.YDIM) & ctp[*,*] = 0 ; cloud top pressure
    cwp = FLTARR(grid.XDIM,grid.YDIM) & cwp[*,*] = 0 ; cloud water path = lwp + iwp
    lwp = FLTARR(grid.XDIM,grid.YDIM) & lwp[*,*] = 0 ; liquid water path
    iwp = FLTARR(grid.XDIM,grid.YDIM) & iwp[*,*] = 0 ; ice water path
    cwp_allsky = FLTARR(grid.XDIM,grid.YDIM) & cwp_allsky[*,*] = 0
    lwp_allsky = FLTARR(grid.XDIM,grid.YDIM) & lwp_allsky[*,*] = 0
    iwp_allsky = FLTARR(grid.XDIM,grid.YDIM) & iwp_allsky[*,*] = 0
    cot = FLTARR(grid.XDIM,grid.YDIM) & cot[*,*] = 0 ; cloud optical thickness
    cot_liq = FLTARR(grid.XDIM,grid.YDIM) & cot_liq[*,*] = 0
    cot_ice = FLTARR(grid.XDIM,grid.YDIM) & cot_ice[*,*] = 0
    cer = FLTARR(grid.XDIM,grid.YDIM) & cer[*,*] = 0 ; cloud effective radius
    cer_liq = FLTARR(grid.XDIM,grid.YDIM) & cer_liq[*,*] = 0
    cer_ice = FLTARR(grid.XDIM,grid.YDIM) & cer_ice[*,*] = 0


    ; -- hist1d [lon,lat,bins,phase] = [720,361,15,2]
    hist1d_ctp = LONARR(grid.XDIM,grid.YDIM,hist.CTP_BIN1D_DIM,hist.PHASE_DIM) 
    hist1d_ctp[*,*,*,*] = 0l
    hist1d_ctt = LONARR(grid.XDIM,grid.YDIM,hist.CTT_BIN1D_DIM,hist.PHASE_DIM) 
    hist1d_ctt[*,*,*,*] = 0l
    hist1d_cwp = LONARR(grid.XDIM,grid.YDIM,hist.CWP_BIN1D_DIM,hist.PHASE_DIM) 
    hist1d_cwp[*,*,*,*] = 0l
    hist1d_cot = LONARR(grid.XDIM,grid.YDIM,hist.COT_BIN1D_DIM,hist.PHASE_DIM) 
    hist1d_cot[*,*,*,*] = 0l
    hist1d_cer = LONARR(grid.XDIM,grid.YDIM,hist.CER_BIN1D_DIM,hist.PHASE_DIM) 
    hist1d_cer[*,*,*,*] = 0l

    ; -- hist2d [lon,lat,cotbins,ctpbins,phase] = [720,361,13,15,2]
    hist2d_cot_ctp = LONARR(grid.XDIM, grid.YDIM, $
                            hist.COT_BIN1D_DIM, hist.CTP_BIN1D_DIM, $
                            hist.PHASE_DIM) 
    hist2d_cot_ctp[*,*,*,*] = 0l

    ; -- counts
    numb_raw = 0l ; files
    numb_cfc = LONARR(grid.XDIM,grid.YDIM) & numb_cfc[*,*] = 0
    numb_ctp = LONARR(grid.XDIM,grid.YDIM) & numb_ctp[*,*] = 0
    numb_cwp = LONARR(grid.XDIM,grid.YDIM) & numb_cwp[*,*] = 0
    numb_lwp = LONARR(grid.XDIM,grid.YDIM) & numb_lwp[*,*] = 0
    numb_iwp = LONARR(grid.XDIM,grid.YDIM) & numb_iwp[*,*] = 0
    numb_cot = LONARR(grid.XDIM,grid.YDIM) & numb_cot[*,*] = 0
    numb_cot_liq = LONARR(grid.XDIM,grid.YDIM) & numb_cot_liq[*,*] = 0
    numb_cot_ice = LONARR(grid.XDIM,grid.YDIM) & numb_cot_ice[*,*] = 0
    numb_cer = LONARR(grid.XDIM,grid.YDIM) & numb_cer[*,*] = 0
    numb_cer_liq = LONARR(grid.XDIM,grid.YDIM) & numb_cer_liq[*,*] = 0
    numb_cer_ice = LONARR(grid.XDIM,grid.YDIM) & numb_cer_ice[*,*] = 0
    numb_cph_day = LONARR(grid.XDIM,grid.YDIM) & numb_cph_day[*,*] = 0
    numb_cwp_allsky = FLTARR(grid.XDIM,grid.YDIM) & numb_cwp_allsky[*,*] = 0
    numb_lwp_allsky = FLTARR(grid.XDIM,grid.YDIM) & numb_lwp_allsky[*,*] = 0
    numb_iwp_allsky = FLTARR(grid.XDIM,grid.YDIM) & numb_iwp_allsky[*,*] = 0


    ; -- create structure
    arrays = { monthly_means, $
               hist2d_cot_ctp:hist2d_cot_ctp, $
               hist1d_ctp:hist1d_ctp, hist1d_ctt:hist1d_ctt, $ 
               hist1d_cwp:hist1d_cwp, hist1d_cot:hist1d_cot, $ 
               hist1d_cer:hist1d_cer, $ 
               cfc:cfc, cph:cph, cph_day:cph_day, $
               ctt:ctt, cth:cth, ctp:ctp, $
               cwp:cwp, lwp:lwp, iwp:iwp, $
               cwp_allsky:cwp_allsky, $
               lwp_allsky:lwp_allsky, $
               iwp_allsky:iwp_allsky, $ 
               cot:cot, cot_liq:cot_liq, cot_ice:cot_ice, $
               cer:cer, cer_liq:cer_liq, cer_ice:cer_ice }

    counts = { counts_monthly_means,$ 
               raw:numb_raw, $
               cfc:numb_cfc, $
               ctp:numb_ctp, $
               cwp:numb_cwp, $
               lwp:numb_lwp, $
               iwp:numb_iwp, $ 
               cwp_allsky:numb_cwp_allsky, $
               lwp_allsky:numb_lwp_allsky, $
               iwp_allsky:numb_iwp_allsky, $ 
               cot:numb_cot, $
               cot_liq:numb_cot_liq, $
               cot_ice:numb_cot_ice, $
               cer:numb_cer, $
               cer_liq:numb_cer_liq, $
               cer_ice:numb_cer_ice, $
               cph_day:numb_cph_day }

END


;-------------------------------------------------------------------
; IN  : DATA
; OUT : GRID
;-------------------------------------------------------------------
PRO GET_ERA_GRID, data, lon, lat, grid
;-------------------------------------------------------------------

    dims = SIZE(data, /DIM)
    xdim = dims[0]
    ydim = dims[1]
    
    ; define longitude & latitude arrays ;[cols, rows]
    longrd=FLTARR(xdim,ydim)
    latgrd=FLTARR(xdim,ydim)
    
    ; create lat/lon grid arrays using lon & lat from ncfile
    FOR loi=0,xdim-1 DO longrd[loi,*]=lon[loi]
    FOR lai=0,ydim-1 DO latgrd[*,lai]=lat[lai]

    grid = { era_grid, $
             lon2d:longrd, lat2d:latgrd, $
             xdim:xdim, ydim:ydim }

END
