;-----------------------------------------------------------------------------
FUNCTION GET_DATE_UTC, filename
;-----------------------------------------------------------------------------
    IF (filename.EndsWith('.nc') EQ 1) THEN BEGIN
        base = FSC_Base_Filename(filename)
        splt = STRSPLIT(base, /EXTRACT, '_')
        time = STRSPLIT(splt[4],/EXTRACT,'+')
        hour = FIX(time[0])
        yyyy = FIX(STRMID(splt[3],0,4))
        mm = FIX(STRMID(splt[3],4,2))
        dd = FIX(STRMID(splt[3],6,2))
        date_utc = splt[3]+' UTC '+splt[4]
        RETURN, date_utc
    ENDIF ELSE BEGIN
        RETURN, STRMID(filename, STRLEN(filename)-6, 6)
    ENDELSE
END


;-----------------------------------------------------------------------------
FUNCTION GET_FILE_NAME, filename, varname, type, scops_type, flag, $ 
                       CONSTANT_CER=creff, RATIO=ratio
;-----------------------------------------------------------------------------
    IF (scops_type EQ 1) THEN st = 'random' ELSE st='max/random'
    ststr = STRTRIM(STRING(scops_type),2)
    basen = FSC_Base_Filename(filename)
    obase = !SAVE_DIR + basen + '_' + flag + '_scops' + ststr
    result = obase + '_'+type+'_'+varname
    IF KEYWORD_SET(creff) THEN result = result + '_fixed_reffs'
    IF KEYWORD_SET(ratio) THEN result = result + '_ratio'
    RETURN, result
END


;-----------------------------------------------------------------------------
FUNCTION GET_ALIQ_AICE, data, sname, varname
;-----------------------------------------------------------------------------

    CASE sname of

        'TEMP_ARRAYS' : BEGIN

            IF (varname EQ 'cot') THEN BEGIN 
                aliq = data.COT_LIQ 
                aice = data.COT_ICE 

            ENDIF ELSE IF (varname EQ 'cer') THEN BEGIN 
                aliq = data.CER_LIQ 
                aice = data.CER_ICE 

            ENDIF ELSE IF (varname EQ 'cwp') THEN BEGIN 
                aliq = data.LWP*1000. ;g/m2
                aice = data.IWP*1000. ;g/m2

            ENDIF ELSE IF (varname EQ 'ctp') THEN BEGIN 
                dims = SIZE(data.CPH, /DIM)
                aliq = FLTARR(dims[0],dims[1]) & aliq[*,*] = -999
                aice = FLTARR(dims[0],dims[1]) & aice[*,*] = -999
                ice = WHERE((data.CTP GT 10.) AND (data.CTH GT 0.)  $
                        AND (data.CPH EQ 0))
                liq = WHERE((data.CTP GT 10.) AND (data.CTH GT 0.)  $
                        AND (data.CPH EQ 1))
                aliq[liq] = data.CTP[liq] 
                aice[ice] = data.CTP[ice]

            ENDIF ELSE IF (varname EQ 'ctt') THEN BEGIN 
                dims = SIZE(data.CPH, /DIM)
                aliq = FLTARR(dims[0],dims[1]) & aliq[*,*] = -999
                aice = FLTARR(dims[0],dims[1]) & aice[*,*] = -999
                ice = WHERE((data.CTP GT 10.) AND (data.CTH GT 0.)  $
                        AND (data.CPH EQ 0))
                liq = WHERE((data.CTP GT 10.) AND (data.CTH GT 0.)  $
                        AND (data.CPH EQ 1))
                aliq[liq] = data.CTT[liq] 
                aice[ice] = data.CTT[ice]

            ENDIF ELSE BEGIN
                PRINT, 'VARNAME not yet defined here.'
                RETURN, 0

            ENDELSE
            END

        ELSE : BEGIN
            PRINT, 'SNAME has an illegal value. Must be TEMP_ARRAYS!'
            RETURN, 0
            END

    ENDCASE

    RETURN, { liq:aliq, ice:aice }
END


;-----------------------------------------------------------------------------
FUNCTION GET_DETAILS, varname, DATA=data, HISTO=histo
;-----------------------------------------------------------------------------
    ; initialize because not all are always required
    vardata = 0. & varhist= 0. & hist_str = 0. & title = 0. 
    maxvalue = 0. & minvalue = 0. & bin_dim = 0. & lim_bin = 0. & bborder = 0
    cci_str = 0. & ymax = 0. & legpos = 0. & binsize = 0.

    CASE varname of

        'cfc' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.CFC
            title = 'Cloud Fraction'
            END
        'cph' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.CPH
            title = 'Cloud Phase'
            END
        'cth' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.CTH
            title = 'Cloud Top Height [km]'
            END
        'lwp' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.LWP
            title = 'Cloud Liquid Water Path [g/m2]'
            END
        'iwp' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.IWP
            title = 'Cloud Ice Water Path [g/m2]'
            END
        'cot_liq' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.COT_LIQ
            title = 'Liquid Cloud Optical Thickness'
            END
        'cot_ice' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.COT_ICE
            title = 'Ice Cloud Optical Thickness'
            END
        'cer_liq' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.CER_LIQ
            title = 'Liquid Cloud Effective Radius [microns]'
            END
        'cer_ice' : BEGIN
            IF KEYWORD_SET(data) THEN vardata = data.CER_ICE
            title = 'Ice Cloud Effective Radius [microns]'
            END

        'ctt' : BEGIN
            IF KEYWORD_SET(data) THEN BEGIN
                vardata = data.CTT
                varhist = data.HIST1D_CTT
            ENDIF
            title = 'Cloud Top Temperature [K]'
            hist_str = 'HIST1D_CTT'
            maxvalue = 350.
            minvalue = 200.
            cci_str = 'ctt'
            ymax = 50
            legpos = "tl" 
            binsize = 5
            IF KEYWORD_SET(histo) THEN BEGIN
                bin_dim = histo.CTT_BIN1D_DIM
                lim_bin = histo.CTT2D
                bborder = histo.CTT1D
            ENDIF
            END

        'ctp' : BEGIN
            IF KEYWORD_SET(data) THEN BEGIN
                vardata = data.CTP
                varhist = data.HIST1D_CTP
            ENDIF
            title = 'Cloud Top Pressue [hPa]'
            hist_str = 'HIST1D_CTP'
            maxvalue = 1100.
            minvalue = 1.
            cci_str = 'ctp'
            ymax = 40
            legpos = "top"
            binsize = 50
            IF KEYWORD_SET(histo) THEN BEGIN
                bin_dim = histo.CTP_BIN1D_DIM
                lim_bin = histo.CTP2D
                bborder = histo.CTP1D
            ENDIF
            END

        'cwp' : BEGIN
            IF KEYWORD_SET(data) THEN BEGIN
                vardata = data.CWP
                varhist = data.HIST1D_CWP
            ENDIF
            title = 'Cloud Water Path [g/m2]'
            hist_str = 'HIST1D_CWP'
            maxvalue = 2000.
            minvalue = 0.0001
            cci_str = 'cwp'
            ymax = 30
            legpos = "tl"
            binsize = 50
            IF KEYWORD_SET(histo) THEN BEGIN
                bin_dim = histo.CWP_BIN1D_DIM
                lim_bin = histo.CWP2D
                bborder = histo.CWP1D
            ENDIF
            END

        'cot' : BEGIN
            IF KEYWORD_SET(data) THEN BEGIN
                vardata = data.COT
                varhist = data.HIST1D_COT
            ENDIF
            title = 'Cloud Optical Thickness'
            hist_str = 'HIST1D_COT'
            maxvalue = 100.
            minvalue = 0.0001
            cci_str = 'cot'
            ymax = 30
            legpos = "tl"
            binsize = 1
            IF KEYWORD_SET(histo) THEN BEGIN
                bin_dim = histo.COT_BIN1D_DIM
                lim_bin = histo.COT2D
                bborder = histo.COT1D
            ENDIF
            END

        'cer' : BEGIN
            IF KEYWORD_SET(data) THEN BEGIN
                vardata = data.CER
                varhist = data.HIST1D_CER
            ENDIF
            title = 'Cloud Effective Radius [microns]'
            hist_str = 'HIST1D_CER'
            maxvalue = 80.
            minvalue = 0.0001
            cci_str = 'ref'
            ymax = 40
            legpos = "tr" 
            binsize = 1
            IF KEYWORD_SET(histo) THEN BEGIN
                bin_dim = histo.CER_BIN1D_DIM
                lim_bin = histo.CER2D
                bborder = histo.CER1D
            ENDIF
            END

    ENDCASE

    RETURN, { data:vardata, hist:varhist, hist_str:hist_str, $
              title:title, maxv:maxvalue, minv:minvalue, $
              bin_dim:bin_dim, lim_bin:lim_bin, bin_border:bborder, $
              cci_str:cci_str, ymax:ymax, legpos:legpos, bin_size:binsize}

END


;-----------------------------------------------------------------------------
PRO SCALE_COT_CWP, data, grd
;-----------------------------------------------------------------------------

    maxcot = 100.
    scale_liq = FLTARR(grd.XDIM,grd.YDIM) & scale_liq[*,*] = 1.
    scale_ice = FLTARR(grd.XDIM,grd.YDIM) & scale_ice[*,*] = 1.

    liq = WHERE( data.COT_LIQ GT maxcot, nliq )
    ice = WHERE( data.COT_ICE GT maxcot, nice )

    IF ( nliq GT 0 ) THEN BEGIN
        scale_liq[liq]   = maxcot / data.COT_LIQ[liq]
        data.LWP[liq]     = data.LWP[liq] * scale_liq[liq]
        data.COT_LIQ[liq] = data.COT_LIQ[liq] * scale_liq[liq]
    ENDIF

    IF ( nice GT 0 ) THEN BEGIN
        scale_ice[ice]   = maxcot / data.COT_ICE[ice]
        data.IWP[ice]     = data.IWP[ice] * scale_ice[ice]
        data.COT_ICE[ice] = data.COT_ICE[ice] * scale_ice[ice]
    ENDIF

END


;-----------------------------------------------------------------------------
PRO SOLAR_VARS, data, sza, grd, FLAG=flag, FILE=fil, MAP=map, SCOPS=type
;-----------------------------------------------------------------------------

    night = WHERE( sza GE 75., nnight, COMPLEMENT=day, NCOMPLEMENT=nday)

    IF ( nnight GT 0 ) THEN BEGIN

        data.LWP[night] = 0.
        data.IWP[night] = 0.
        data.COT_LIQ[night] = 0.
        data.COT_ICE[night] = 0.
        data.CER_LIQ[night] = 0.
        data.CER_ICE[night] = 0.

    ENDIF

    IF KEYWORD_SET(map) AND KEYWORD_SET(fil) AND KEYWORD_SET(flag) THEN $ 
        PLOT_SOLAR_VARS, DATA=data, GRID=grd, FLAG=flag, $
                         FILE=fil, VOID=night, SCOPS=type

END


;-----------------------------------------------------------------------------
PRO SPLIT_ERA_FILENAME, FILE=file, BASE=basename, DIR=dir, EXT=ext, $
    YEAR=year, MONTH=month, DAY=day, HOUR=hour, UTC=utc
;-----------------------------------------------------------------------------
    basename = FSC_Base_Filename(file, DIR=dir, EXT=ext)
    splt = STRSPLIT(basename, /EXTRACT, '_')
    time = STRSPLIT(splt[4],/EXTRACT,'+')
    hour = time[0]
    year = STRMID(splt[3],0,4)
    month = STRMID(splt[3],4,2)
    day = STRMID(splt[3],6,2)
    utc = splt[4]
END


;------------------------------------------------------------------------------
FUNCTION SUMUP_HIST1D, bin_dim=bin1d_dim, cph_dim=phase_dim, lim_bin=bbins, $
                       var_tmp=var, liq_tmp=liq, ice_tmp=ice, cfc_tmp=cfc, $
                       cph_tmp=phase
;------------------------------------------------------------------------------
; -- NOTE --
; simulator -> 0=ice,    1=liquid
; cc4cl     -> 0=liquid, 1=ice
;------------------------------------------------------------------------------

    IF KEYWORD_SET(liq) AND KEYWORD_SET(ice)  $
        AND ~KEYWORD_SET(phase) THEN BEGIN 

        dims = SIZE(liq, /DIM)

    ENDIF ELSE IF KEYWORD_SET(var) $
        AND KEYWORD_SET(phase) THEN BEGIN

        dims = SIZE(var, /DIM)

    ENDIF

    IF ( N_ELEMENTS(dims) EQ 1 ) THEN dims = [dims[0],1]

    ; counts [lon,lat]
    cnts = LONARR(dims[0],dims[1])
    cnts[*,*] = 0l 

    ; hist1d [lon,lat,bins,phase] = [720,361,15,2]
    vmean = LONARR(dims[0],dims[1],bin1d_dim,phase_dim) 
    vmean[*,*,*,*] = 0l

    ; last bin
    gu_last = bin1d_dim-1

    FOR gu=0, gu_last DO BEGIN 

        ; consider also last bin-border via GE & LE
        IF ( gu EQ gu_last ) THEN BEGIN

            IF KEYWORD_SET(liq) AND KEYWORD_SET(ice) $
                AND ~KEYWORD_SET(phase) THEN BEGIN

                wohi_ice = WHERE( ice GE bbins[0,gu] AND $ 
                                  ice LE bbins[1,gu] AND $
                                  cfc GT 0. , nwohi_ice )

                wohi_liq = WHERE( liq GE bbins[0,gu] AND $ 
                                  liq LE bbins[1,gu] AND $
                                  cfc GT 0. , nwohi_liq )


            ENDIF ELSE IF KEYWORD_SET(var) AND KEYWORD_SET(phase) THEN BEGIN

                wohi_ice = WHERE( var GE bbins[0,gu] AND $ 
                                  var LE bbins[1,gu] AND $
                                  cfc EQ 1. AND phase EQ 0., $
                                  nwohi_ice )

                wohi_liq = WHERE( var GE bbins[0,gu] AND $ 
                                  var LE bbins[1,gu] AND $
                                  cfc EQ 1. AND phase EQ 1., $
                                  nwohi_liq )

            ENDIF

        ; between GE & LT
        ENDIF ELSE BEGIN

            IF KEYWORD_SET(liq) AND KEYWORD_SET(ice) $
                AND ~KEYWORD_SET(phase) THEN BEGIN

                wohi_ice = WHERE( ice GE bbins[0,gu] AND $ 
                                  ice LT bbins[1,gu] AND $
                                  ice GT 0. AND $
                                  cfc GT 0. , nwohi_ice )

                wohi_liq = WHERE( liq GE bbins[0,gu] AND $ 
                                  liq LT bbins[1,gu] AND $
                                  liq GT 0. AND $
                                  cfc GT 0. , nwohi_liq )


            ENDIF ELSE IF KEYWORD_SET(var) AND KEYWORD_SET(phase) THEN BEGIN
                
                wohi_ice = WHERE( var GE bbins[0,gu] AND $ 
                                  var LT bbins[1,gu] AND $
                                  var GT 0. AND $
                                  cfc EQ 1. AND phase EQ 0., $
                                  nwohi_ice )

                wohi_liq = WHERE( var GE bbins[0,gu] AND $ 
                                  var LT bbins[1,gu] AND $
                                  var GT 0. AND $
                                  cfc EQ 1. AND phase EQ 1., $
                                  nwohi_liq )

            ENDIF

        ENDELSE


        IF ( nwohi_ice GT 0 ) THEN BEGIN
            cnts[wohi_ice] = 1l
            vmean[*,*,gu,1] = vmean[*,*,gu,1] + cnts
            cnts[*,*] = 0l 
        ENDIF


        IF ( nwohi_liq GT 0 ) THEN BEGIN
            cnts[wohi_liq] = 1l
            vmean[*,*,gu,0] = vmean[*,*,gu,0] + cnts
            cnts[*,*] = 0l 
        ENDIF


    ENDFOR

    RETURN, vmean

END


;------------------------------------------------------------------------------
FUNCTION SUMUP_HIST2D, hist, cot, ctp, cfc, cph
;------------------------------------------------------------------------------
; sum up 2d histograms: 
; -- NOTE --
; simulator -> 0=ice,    1=liquid
; cc4cl     -> 0=liquid, 1=ice
;------------------------------------------------------------------------------

    dims = SIZE(cot, /DIM)
    IF ( N_ELEMENTS(dims) EQ 1 ) THEN dims = [dims[0],1]

    ; counts [lon,lat]
    cnts = LONARR(dims[0],dims[1])
    cnts[*,*] = 0l 

    ; hist2d [lon,lat,cotbins,ctpbins,phase] = [720,361,13,15,2]
    vmean = LONARR(dims[0], dims[1], hist.cot_bin1d_dim, $
                   hist.ctp_bin1d_dim, hist.phase_dim) 
    vmean[*,*,*,*] = 0l

    ; last bins
    ctp_last = hist.ctp_bin1d_dim-1
    cot_last = hist.cot_bin1d_dim-1

    FOR ictp=0, ctp_last DO BEGIN 
        FOR jcot=0, cot_last DO BEGIN

            ; consider also last COT & CTP bin-border via GE & LE
            IF ( jcot EQ cot_last AND ictp EQ ctp_last) THEN BEGIN

                wohi_ice = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LE hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LE hist.ctp2d[1,ictp] AND $
                                  cfc EQ 1. AND cph EQ 0., nwohi_ice )

                wohi_liq = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LE hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LE hist.ctp2d[1,ictp] AND $
                                  cfc EQ 1. AND cph EQ 1., nwohi_liq )


            ; consider also last COT bin-border via GE & LE
            ENDIF ELSE IF ( jcot EQ cot_last ) THEN BEGIN

                wohi_ice = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LE hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LT hist.ctp2d[1,ictp] AND $
                                  cfc EQ 1. AND cph EQ 0., nwohi_ice )

                wohi_liq = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LE hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LT hist.ctp2d[1,ictp] AND $
                                  cfc EQ 1. AND cph EQ 1., nwohi_liq )


            ; consider also last CTP bin-border via GE & LE
            ENDIF ELSE IF ( ictp EQ ctp_last) THEN BEGIN


                wohi_ice = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LT hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LE hist.ctp2d[1,ictp] AND $
                                  cfc EQ 1. AND cph EQ 0., nwohi_ice )

                wohi_liq = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LT hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LE hist.ctp2d[1,ictp] AND $
                                  cfc EQ 1. AND cph EQ 1., nwohi_liq )


            ; between GE & LT
            ENDIF ELSE BEGIN

                wohi_ice = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LT hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LT hist.ctp2d[1,ictp] AND $
                                  cot GT 0. AND ctp GT 0. AND $
                                  cfc EQ 1. AND cph EQ 0., nwohi_ice )

                wohi_liq = WHERE( cot GE hist.cot2d[0,jcot] AND $ 
                                  cot LT hist.cot2d[1,jcot] AND $
                                  ctp GE hist.ctp2d[0,ictp] AND $
                                  ctp LT hist.ctp2d[1,ictp] AND $
                                  cot GT 0. AND ctp GT 0. AND $
                                  cfc EQ 1. AND cph EQ 1., nwohi_liq )

            ENDELSE

            ; hist2d [lon,lat,cotbins,ctpbins,phase] = [720,361,13,15,2]

            IF ( nwohi_ice GT 0 ) THEN BEGIN
                cnts[wohi_ice] = 1l
                vmean[*,*,jcot,ictp,1] = vmean[*,*,jcot,ictp,1] + cnts
                cnts[*,*] = 0l 
            ENDIF

            IF ( nwohi_liq GT 0 ) THEN BEGIN
                cnts[wohi_liq] = 1l
                vmean[*,*,jcot,ictp,0] = vmean[*,*,jcot,ictp,0] + cnts
                cnts[*,*] = 0l 
            ENDIF

        ENDFOR
    ENDFOR

    RETURN, vmean

END


;-----------------------------------------------------------------------------
PRO PLOT_ERA_SST, FILENAME=filename, DATA=sst, $
                  LATITUDE=lat, LONGITUDE=lon, VOID=void_index
;-----------------------------------------------------------------------------
    !EXCEPT=0

    filepwd = !SAVE_DIR + filename

    IF ( is_file(filepwd+'.png') ) THEN RETURN

    save_as = filepwd + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE

    limit = [-90., -180., 90., 180.]

    MAP_IMAGE, sst, lat, lon, LIMIT=limit, $
               CTABLE=33, /BOX_AXES, /MAGNIFY, /GRID, $
               FORMAT=('(f5.1)'), N_LEV=6, $
               MINI=MIN(sst), MAXI=MAX(sst), $
               CHARSIZE=2.2, VOID_INDEX=void_index, $
               TITLE='SST [K]', $
               FIGURE_TITLE="ERA-Interim Sea Surface Temperature"

    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2

    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2

    end_save, save_as
    cs_eps2png, save_as

END


;-----------------------------------------------------------------------------
PRO PLOT_LSM2D, FILENAME=filename, DATA=lsm, $
                LATITUDE=lat, LONGITUDE=lon, TITLE=title
;-----------------------------------------------------------------------------
    !EXCEPT=0

    filepwd = !SAVE_DIR + filename

    IF ( is_file(filepwd+'.png') ) THEN RETURN

    save_as = filepwd + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE

    limit = [-90., -180., 90., 180.]
    bar_tickname = ['Water', 'Land']
    nlev = N_ELEMENTS(bar_tickname)
    discrete = FINDGEN(N_ELEMENTS(bar_tickname)+1)

    MAP_IMAGE, lsm, lat, lon, LIMIT=limit, $
               CTABLE=33, /BOX_AXES, /MAGNIFY, /GRID, $
               DISCRETE=discrete, N_LEV=nlev, $
               BAR_TICKNAME=bar_tickname, $
               MINI=MIN(lsm), MAXI=MAX(lsm), $
               CHARSIZE=2.2, VOID_INDEX=void_index, $
               FIGURE_TITLE=title;+'!C'

    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2

    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2

    end_save, save_as
    cs_eps2png, save_as

END


;-----------------------------------------------------------------------------
PRO PLOT_SZA2D, FILENAME=filename, DATA=sza2d, $
                LATITUDE=lat, LONGITUDE=lon, TITLE=title
;-----------------------------------------------------------------------------
    !EXCEPT=0

    filepwd = !SAVE_DIR + filename + '_sza'

    IF ( is_file(filepwd+'.png') ) THEN RETURN

    save_as = filepwd + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE
    limit = [-90., -180., 90., 180.]


    MAP_IMAGE, sza2d, lat, lon, LIMIT=limit, $
               CTABLE=33, /FLIP_COLOURS, $
               /BOX_AXES, /MAGNIFY, /GRID, $
               MINI=0., MAXI=180., CHARSIZE=2.2, $
               TITLE='SZA [deg]', $
               FIGURE_TITLE=title

    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2

    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2

    end_save, save_as
    cs_eps2png, save_as

END


;-----------------------------------------------------------------------------
PRO MAP_MM, grd, data, filename, varname, flag, scops_type, $
            TITLE=title, CONSTANT_CER=creff
;-----------------------------------------------------------------------------
    !EXCEPT=0

    ofil = GET_FILE_NAME( filename, varname, 'map', scops_type, flag, $
                         CONSTANT_CER=creff )

    IF (is_file(ofil+'.png')) THEN RETURN

    save_as = ofil + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE
    limit = [-90., -180., 90., 180.]
    plt = GET_DETAILS( varname, DATA=data )
    datutc = GET_DATE_UTC(filename)
    void = WHERE(plt.DATA LT 0, COMPLEMENT=good)

    MAP_IMAGE, plt.DATA, grd.LAT2D, grd.LON2D, LIMIT=limit, $
               CTABLE=33, /BOX_AXES, /MAGNIFY, /GRID, $
               MINI=MIN(plt.DATA[good]), MAXI=MAX(plt.DATA[good]), $
               CHARSIZE=2.0, TITLE=plt.TITLE, VOID_INDEX=void, $
               FORMAT=('(f8.1)'), N_LEV=6, FIGURE_TITLE=datutc

    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2

    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2

    end_save, save_as
    cs_eps2png, save_as

END


;-----------------------------------------------------------------------------
PRO PLOT_HISTOS_1D, varname, data, histo, filename, flag, scops_type, $
                    CONSTANT_CER=creff, RATIO=ratio
;-----------------------------------------------------------------------------

    ofil = GET_FILE_NAME( filename, varname, 'hist1d', scops_type, flag, $
                         CONSTANT_CER=creff, RATIO=ratio )

    IF (is_file(ofil+'.png')) THEN RETURN

    IF (scops_type EQ 1) THEN st = 'random' ELSE st='max/random'
    datutc = GET_DATE_UTC(filename)
    datutc = ' for ' + datutc + ' (SCOPS type: '+st+')'
    plt = GET_DETAILS( varname, DATA=data, HISTO=histo )

    ; start plotting
    save_as = ofil + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE
    cs = 2.0

    CREATE_1DHIST, RESULT=plt.HIST, VARNAME=plt.CCI_STR, $
        VARSTRING=plt.HIST_STR, CHARSIZE=cs, XTITLE=datutc, $
        YMAX=plt.YMAX, LEGEND_POSITION=plt.LEGPOS, RATIO=ratio, $
        BIN_BORDERS=plt.BIN_BORDER

    ; end plotting
    end_save, save_as
    cs_eps2png, save_as

END


;-----------------------------------------------------------------------------
PRO PLOT_INTER_HISTOS, data, varname, histo, filename, flag, scops_type, $
                       CONSTANT_CER=creff, RATIO=ratio
;-----------------------------------------------------------------------------

    ofil = GET_FILE_NAME( filename, varname, '2Dtmp', scops_type, flag, $
                         CONSTANT_CER=creff, RATIO=ratio )

    IF ( is_file( ofil + '.png' ) ) THEN RETURN

    sname = SIZE(data, /SNAME)
    cph_dim = histo.PHASE_DIM

    IF (scops_type EQ 1) THEN st = 'random' ELSE st='max/random'
    astr = ' (2D - upper most clouds; temps.CFC) '
    varstring = 'temps'

    ; collect data and hist settings
    varplt = GET_ALIQ_AICE( data, sname, varname )
    auxplt = GET_DETAILS( varname, HISTO=histo )
    datutc = GET_DATE_UTC( filename )
    datutc = ' for ' + datutc

    ; start plotting
    !P.MULTI = [0,2,2]
    save_as = ofil + '.eps'
    start_save, save_as, size=[45,30]
    cs = 2.0
    lg = 'tr'

    ; get total COT
    all = (varplt.LIQ>0) + (varplt.ICE>0) ; consider fill_values

    ; VARNAME GT maxvalue set to maxvalue
    i1=WHERE( varplt.LIQ GT auxplt.MAXV, ni1)
    IF (ni1 GT 0) THEN varplt.LIQ[i1] = auxplt.MAXV 
    i2=WHERE( varplt.ICE GT auxplt.MAXV, ni2)
    IF (ni2 GT 0) THEN varplt.ICE[i2] =  auxplt.MAXV
    i3=WHERE( all GT auxplt.MAXV, ni3)
    IF (ni3 GT 0) THEN all[i3] = auxplt.MAXV


    ; -- HIST1D: equal binsizes ---
    cgHistoplot, varplt.LIQ, binsize=auxplt.BIN_SIZE, /FILL, $
        POLYCOLOR='red', mininput=auxplt.MINV, maxinput=auxplt.MAXV, $
        charsize=cs, histdata=cghist_liq, $
        xtitle=varstring+'.'+STRUPCASE(varname)+'_LIQ'+astr
    legend, ['binsize='+STRTRIM(STRING(auxplt.BIN_SIZE,FORMAT='(I)'),2)], $
        thick=4., spos=lg, charsize=cs, color=[cgcolor("red")]

    cgHistoplot, varplt.ICE, binsize=auxplt.BIN_SIZE, /FILL, $
        POLYCOLOR='royal blue', histdata=cghist_ice, $
        mininput=auxplt.MINV, maxinput=auxplt.MAXV, charsize=cs, $
        xtitle=varstring+'.'+STRUPCASE(varname)+'_ICE'+astr
    legend, ['binsize='+STRTRIM(STRING(auxplt.BIN_SIZE,FORMAT='(I)'),2)], $
        thick=4., spos=lg, charsize=cs, color=[cgcolor("royal blue")]

    cgHistoplot, all, binsize=auxplt.BIN_SIZE, /FILL, $
        POLYCOLOR='black', histdata=cghist_all, $
        mininput=auxplt.MINV, maxinput=auxplt.MAXV, charsize=cs, $
        xtitle=varstring+'.'+STRUPCASE(varname)+astr
    legend, ['binsize='+STRTRIM(STRING(auxplt.BIN_SIZE,FORMAT='(I)'),2)], $
        thick=4., spos=lg, charsize=cs, color=[cgcolor("black")]
    
    ; -- HIST1D: cloud_cci binsizes ---
    res = SUMUP_HIST1D( bin_dim=auxplt.BIN_DIM, lim_bin=auxplt.LIM_BIN, $
                        cph_dim=cph_dim, cfc_tmp=data.CFC, $
                        liq_tmp=varplt.LIQ, ice_tmp=varplt.ICE )

    CREATE_1DHIST, RESULT=res, VARNAME=auxplt.CCI_STR, YMAX=auxplt.YMAX, $
        VARSTRING=varstring, CHARSIZE=1.6, XTITLE=datutc, RATIO=ratio, $
        LEGEND_POSITION=auxplt.LEGPOS, BIN_BORDERS=auxplt.BIN_BORDER

    cgText, 0.52, 0.49, Alignment=0.5, /Normal, 'SCOPS.type='+st, chars=cs

    ; end plotting
    end_save, save_as
    cs_eps2png, save_as
    !P.MULTI = 0

END


;-----------------------------------------------------------------------------
PRO PLOT_SOLAR_VARS, DATA=data, GRID=grd, FLAG=flg, FILE=fil, VOID=void,$
                     SCOPS=type
;-----------------------------------------------------------------------------
    !EXCEPT=0

    ststr = STRTRIM(STRING(type),2)
    base = FSC_Base_Filename(fil)
    filepwd = !SAVE_DIR + base + '_' + flg + '_scops'+ ststr + '_daytime'

    IF ( is_file(filepwd+'.png') ) THEN RETURN

    !P.MULTI = [0,2,3]
    save_as = filepwd + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE

    cs = 1.5 & cs_bar = 2 & nlev = 6
    limit = [-90., -180., 90., 180.]
    
    MAP_IMAGE, data.LWP*1000., grd.LAT2D, grd.LON2D, $
        /MAGNIFY, CHARS=cs_bar, FORMAT=('(f8.1)'), N_LEV=nlev, $
        MINI=MIN(data.LWP*1000.), MAXI=MAX(data.LWP*1000.), $
        VOID_INDEX=void, /RAINBOW, LIMIT=limit, TITLE='LWP [g/m2]'
    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2, /BOX_AXES, CHARS=cs

    MAP_IMAGE, data.IWP*1000., grd.LAT2D, grd.LON2D, $
        /MAGNIFY, CHARS=cs_bar, FORMAT=('(f8.1)'), N_LEV=nlev, $
        MINI=MIN(data.IWP*1000.), MAXI=MAX(data.IWP*1000.), $
        VOID_INDEX=void, /RAINBOW, LIMIT=limit, TITLE='IWP [g/m2]'
    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2, /BOX_AXES, CHARS=cs

    MAP_IMAGE, data.CER_LIQ, grd.LAT2D, grd.LON2D, $
        /MAGNIFY, CHARS=cs_bar, TITLE='CER_LIQ [microns]', $
        MINI=MIN(data.CER_LIQ), MAXI=MAX(data.CER_LIQ), $
        VOID_INDEX=void, /RAINBOW, N_LEV=nlev, LIMIT=limit
    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2, /BOX_AXES, CHARS=cs

    MAP_IMAGE, data.CER_ICE, grd.LAT2D, grd.LON2D, $
        /MAGNIFY, CHARS=cs_bar, TITLE='CER_ICE [microns]', $
        MINI=MIN(data.CER_ICE), MAXI=MAX(data.CER_ICE), $
        VOID_INDEX=void, /RAINBOW, N_LEV=nlev, LIMIT=limit
    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2, /BOX_AXES, CHARS=cs

    MAP_IMAGE, data.COT_LIQ, grd.LAT2D, grd.LON2D, $
        /MAGNIFY, CHARS=cs_bar, TITLE='COT_LIQ', $
        MINI=0., MAXI=30., $
        ;MINI=MIN(data.COT_LIQ), MAXI=MAX(data.COT_LIQ), $
        VOID_INDEX=void, /RAINBOW, N_LEV=nlev, LIMIT=limit
    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2, /BOX_AXES, CHARS=cs

    MAP_IMAGE, data.COT_ICE, grd.LAT2D, grd.LON2D, $
        /MAGNIFY, CHARS=cs_bar, TITLE='COT_ICE', $
        MINI=0., MAXI=60., $
        ;MINI=MIN(data.COT_ICE), MAXI=MAX(data.COT_ICE), $
        VOID_INDEX=void, /RAINBOW, N_LEV=nlev, LIMIT=limit
    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2, /BOX_AXES, CHARS=cs

    end_save, save_as
    cs_eps2png, save_as

    !P.MULTI = 0
END


;-----------------------------------------------------------------------------
PRO CREATE_1DHIST, RESULT=res, VARNAME=vn, VARSTRING=vs, $
                   CHARSIZE=cs, XTITLE=xtitle, YMAX=ymax, $
                   LEGEND_POSITION=lp, RATIO=ratio, BIN_BORDERS=xtickname,$
                   COMPARE=compare
;-----------------------------------------------------------------------------
    IF NOT KEYWORD_SET(ymax) THEN ymax = 40
    IF NOT KEYWORD_SET(lp) THEN lp='tl'
    IF KEYWORD_SET(ratio) THEN ymax = 100

    IF KEYWORD_SET(compare) THEN BEGIN
        ; get reference data
        ref_liq = GET_DATA(compare.YEAR, compare.MONTH, sat=compare.SAT, $
                           algo=compare.REF, level='l3c', $
                           data='hist1d_' + compare.VAR + '_liq') 
        ref_ice = GET_DATA(compare.YEAR, compare.MONTH, sat=compare.SAT, $
                           algo=compare.REF, level='l3c', $
                           data='hist1d_' + compare.VAR + '_ice') 
        ; consider fill_values
        ref_all = ( ref_liq>0 ) + ( ref_ice>0 )

        ; prepare plotting results
        refbild = get_1d_rel_hist_from_1d_hist( ref_all, $
                    'hist1d_'+compare.VAR, algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic, $
                    ytitle = ytitle, hist_name=data_name, found=found1)

        refbild1 = get_1d_rel_hist_from_1d_hist( ref_liq, $
                    'hist1d_'+compare.VAR+'_liq', algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                    ytitle = ytitle, hist_name=data_name, found=found1)

        refbild2 = get_1d_rel_hist_from_1d_hist( ref_ice, $
                    'hist1d_'+compare.VAR+'_ice', algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                    ytitle = ytitle, hist_name=data_name, found=found1)

        IF KEYWORD_SET(ratio) THEN BEGIN
            ref_total = GET_DATA(compare.YEAR, compare.MONTH, sat=compare.SAT, $
                                 algo=compare.REF, level='l3c', $
                                 data='hist1d_' + compare.VAR) 
            refbild3 = get_1d_rel_hist_from_1d_hist( ref_total, $
                    'hist1d_'+compare.VAR+'_ratio', algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                    ytitle = ytitle, hist_name=data_name, found=found1)
        ENDIF
    ENDIF


    bild_liq = reform(res[*,*,*,0])
    bild_ice = reform(res[*,*,*,1])
    bild_all = ( bild_liq>0 ) + ( bild_ice>0 )

    bild = get_1d_rel_hist_from_1d_hist( bild_all, 'hist1d_'+vn, $
                algo='era-i', land=land, sea=sea, arctic=arctic, $
                antarctic=antarctic, ytitle = ytitle, hist_name=data_name, $
                found=found1)

    bild1 = get_1d_rel_hist_from_1d_hist( bild_liq, $
                'hist1d_'+vn+'_liq', algo='era-i', $
                land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                ytitle = ytitle, hist_name=data_name, $
                found=found1)

    bild2 = get_1d_rel_hist_from_1d_hist( bild_ice, $
                'hist1d_'+vn+'_ice', algo='era-i', $
                land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                ytitle = ytitle, hist_name=data_name, $
                found=found1)

    IF KEYWORD_SET(ratio) THEN BEGIN
        bild3 = get_1d_rel_hist_from_1d_hist( res, $
                'hist1d_'+vn+'_ratio', algo='era-i', $
                land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                ytitle = ytitle, hist_name=data_name, $
                found=found1)
    ENDIF

    IF ~KEYWORD_SET(xtitle) THEN xtit = data_name ELSE xtit = xtitle

    plot,[0,0],[1,1],yr=[0,ymax],xr=[0,N_ELEMENTS(bild)-1],$
        xticks=N_ELEMENTS(xtickname)-1, col=cgcolor('black'), $
        xtickname = strcompress(string(xtickname,f='(f20.1)'),/rem), $
        xtitle=xtit, ytitle=ytitle, xminor=2, charsize=cs

    lsty = 0
    thick = 4
    oplot,bild, psym=-1, col=cgcolor('Black'), THICK=thick
    oplot,bild1,psym=-2, col=cgcolor('Red'), THICK=thick
    oplot,bild2,psym=-4, col=cgcolor('royal blue'), THICK=thick
    IF KEYWORD_SET(ratio) THEN $
        oplot,bild3,psym=-6, col=cgcolor('forest green'), THICK=thick

    IF KEYWORD_SET(compare) THEN BEGIN
        oplot,refbild, psym=-1, col=cgcolor('Black'), THICK=thick, LINE=3
        oplot,refbild1,psym=-2, col=cgcolor('Red'), THICK=thick, LINE=3
        oplot,refbild2,psym=-4, col=cgcolor('royal blue'), THICK=thick, LINE=3
        IF KEYWORD_SET(ratio) THEN $
            oplot,refbild3,psym=-6, col=cgcolor('forest green'), THICK=thick, LINE=3

        allstr  = compare.REF+'_'+vs
        aliqstr = compare.REF+'_'+vs+'.LIQ'
        aicestr = compare.REF+'_'+vs+'.ICE'
        ratiostr = compare.REF+'_'+vs+'.RATIO'
        labels = [allstr,aliqstr,aicestr]
        IF KEYWORD_SET(ratio) THEN labels = [labels, ratiostr]
        colors = [cgcolor("Black"),cgcolor("Red"),cgcolor("royal blue")]
        IF KEYWORD_SET(ratio) THEN colors = [colors, cgcolor("forest green")]
        nlabels = N_ELEMENTS(labels)
        legend, labels, thick=REPLICATE(thick,nlabels), spos='tr', $
                charsize=cs, color=colors, line=REPLICATE(3,nlabels)
    ENDIF

    allstr  = vs
    aliqstr = vs+'.LIQ'
    aicestr = vs+'.ICE'
    ratiostr = vs+'.RATIO'
    labels = [allstr,aliqstr,aicestr]
    IF KEYWORD_SET(ratio) THEN labels = [labels, ratiostr]
    colors = [cgcolor("Black"),cgcolor("Red"),cgcolor("royal blue")]
    IF KEYWORD_SET(ratio) THEN colors = [colors, cgcolor("forest green")]
    nlabels = N_ELEMENTS(labels)

    legend, labels, thick=REPLICATE(thick,nlabels), spos=lp, $
            charsize=cs, color=colors

END


;-----------------------------------------------------------------------------
PRO PLOT_SIM_HIST, file, varname, save_dir, base, xtitle, units, time, $
                   SAT=sat, REFS=refs, RATIO=ratio
;-----------------------------------------------------------------------------

    CASE varname OF
        'cer': ymax=50.
        'cot': ymax=30.
        'cwp': ymax=30.
        'ctt': ymax=50.
        'ctp': ymax=40.
        ELSE: UNDEFINE, ymax
    ENDCASE
    
    IF (ymax NE !NULL) THEN BEGIN
    
        opt = 'hist1d_'
        outfile = save_dir + base + '_' + opt + varname 
        IF KEYWORD_SET(ratio) THEN outfile = outfile + '_ratio'
    
        READ_SIM_NCDF, h1d, FILE=file, VAR_NAME=opt+varname
        READ_SIM_NCDF, bin, FILE=file, VAR_NAME=opt+varname+'_bin_border'
        
        save_as = outfile + '.eps'
        start_save, save_as, size=[35,20]
        cs = 2.3
    
        IF (varname EQ 'cer') THEN varn = 'ref' ELSE varn = varname
    
        CREATE_1DHIST, RESULT=h1d, VARNAME=varn, $
            VARSTRING=opt+varn, CHARSIZE=cs, XTITLE=xtitle, $
            YMAX=ymax, LEGEND_POSITION=legend_position, RATIO=ratio, $
            BIN_BORDERS=bin
    
        end_save, save_as
    
        IF KEYWORD_SET(refs) THEN BEGIN
            FOR r=0, N_ELEMENTS(refs)-1 DO BEGIN
                opt = 'hist1d_'
                outfile = save_dir + base + '_' + opt + varname
                IF KEYWORD_SET(ratio) THEN outfile = outfile + '_ratio'
                outfile = outfile + '_compare_with_'+refs[r]
    
                save_as = outfile + '.eps'
                start_save, save_as, size=[35,20]
                cs = 2.3
    
                compare = {ref:refs[r], sat:sat, var:varn, $
                           year:STRMID(time, 0, 4), $
                           month:STRMID(time, 4, 2), dat:''}
    
                CREATE_1DHIST, RESULT=h1d, VARNAME=varn, $
                    VARSTRING=opt+varn, CHARSIZE=cs, XTITLE=xtitle, $
                    YMAX=ymax, LEGEND_POSITION=legend_position, RATIO=ratio, $
                    BIN_BORDERS=bin, COMPARE=compare
    
                end_save, save_as
            ENDFOR
        ENDIF

    ENDIF

END


;-----------------------------------------------------------------------------
PRO PLOT_SIM_MAPS, file, varname, save_dir, data, fillvalue, mini, maxi, $
                   base, figure_title, xtitle
;-----------------------------------------------------------------------------
    opt = 'map_'
    outfile = save_dir + base + '_' + opt + varname
    save_as = outfile + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE

    limit = [-90., -180., 90., 180.]
    void = WHERE(data EQ fillvalue, COMPLEMENT=good)
    
    READ_SIM_NCDF, lon, FILE=file, VAR_NAME='lon'
    READ_SIM_NCDF, lat, FILE=file, VAR_NAME='lat'
    GET_ERA_GRID, data, lon, lat, grid
    
    MAP_IMAGE, data, grid.LAT2D, grid.LON2D, LIMIT=limit, $
               CTABLE=33, /BOX_AXES, /MAGNIFY, /GRID, $
               MINI=mini, MAXI=maxi, $
               CHARSIZE=2.0, TITLE=xtitle, VOID_INDEX=void, $
               FORMAT=('(f8.1)'), N_LEV=6, $
               FIGURE_TITLE=figure_title
    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2
    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2
    
    end_save, save_as
END


;-----------------------------------------------------------------------------
PRO PLOT_SIM_COMPARE_WITH, file, refs, varname, save_dir, time, $
                           mini, maxi, SAT=sat
;-----------------------------------------------------------------------------
    FOR r=0, N_ELEMENTS(refs)-1 DO BEGIN
        PRINT, "** Compare simulated "+varname+" with "+refs[r]
    
        year = STRMID(time, 0, 4)
        month = STRMID(time, 4, 2) 
    
        CASE varname OF
            'cer': cci_varn = 'ref'
            'cer_liq': cci_varn = 'ref_liq'
            'cer_ice': cci_varn = 'ref_ice'
            ELSE: cci_varn = varname
        ENDCASE
    
        compare_cci_with_clara, year, month, '', ALGO1='era-i',$
            DATA=cci_varn, CCIFILE=file, REFERENCE=refs[r], $
            SAT=sat, MINI=mini, MAXI=maxi , LIMIT=limit, $
            SAVE_DIR=save_dir, LAND=land, SEA=sea, COV=cov, $
            OTHER='rainbow', CTABLE='', LEVEL='l3c'
    ENDFOR
END


;-----------------------------------------------------------------------------
PRO PLOT_SIM_COMPARE_ZONAL, file, vname, time, save_dir, base, $
                            mini, maxi, REFS=refs, SAT=sat
;-----------------------------------------------------------------------------
    PRINT, "** Compare zonal means"

    opt = 'zonal_'
    outfile = save_dir + base + '_' + opt + vname
    save_as = outfile + '.eps'
    start_save, save_as, size=[32,20]

    year = STRMID(time, 0, 4)
    month = STRMID(time, 4, 2) 
    datum = year + month
    
    CASE vname OF
        'cer':      BEGIN & maxv=maxi & varname = 'ref' & END
        'cer_liq':  BEGIN & maxv=maxi & varname = 'ref_liq' & END
        'cer_ice':  BEGIN & maxv=maxi & varname = 'ref_ice' & END
        'cfc' :     BEGIN & maxv=maxi+maxi/10. & varname=vname & END
        'cph' :     BEGIN & maxv=maxi+maxi/10. & varname=vname & END
        ELSE:       BEGIN & varname=vname & maxv=maxi & END
    ENDCASE
    
    opl = 0
    plot_zonal_average, year, month, '', $
        file, varname, algo='era-i', sea=sea, land=land, $
        limit=limit, mini=mini, maxi=maxv, $
        found=found, level=level, datum=datum, error=error, $
        oplots=opl, addtext='Simulated', /simulator
    
    IF KEYWORD_SET(refs) THEN BEGIN
        FOR r=0, N_ELEMENTS(refs)-1 DO BEGIN
            plot_zonal_average, year, month, '', $
                '', varname, algo=refs[r], sea=sea, land=land, $
                limit=limit, mini=mini, maxi=maxi, satellite=sat, $
                found=found, level=level, datum=datum, error=error, $
                oplots=r+opl+1, /simulator
        ENDFOR
    ENDIF

    end_save, save_as

END
