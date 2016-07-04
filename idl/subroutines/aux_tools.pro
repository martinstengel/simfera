;-----------------------------------------------------------------------------
FUNCTION GET_AVAILABILITY, var_name, ref_algo
;-----------------------------------------------------------------------------
    CASE ref_algo OF
        'pmx': BEGIN
            CASE var_name OF
                'hist2d_cot_ctp': avail = -1
                'hist1d_ctp': avail = -1
                'hist1d_ctt': avail = -1
                'hist1d_cwp': avail = -1
                'hist1d_cot': avail = -1
                'hist1d_cer': avail = -1
                'cwp': avail = -1
                'cth': avail = -1
                'cer': avail = -1
                'cph': avail = -1
                'cwp_allsky': avail = -1
                'lwp_allsky': avail = -1
                'iwp_allsky': avail = -1
                ELSE: avail = 1
            ENDCASE
            END
        'mod': BEGIN
            CASE var_name OF
                'hist2d_cot_ctp': avail = -1
                'hist1d_ctp'    : avail = -1
                'cth': avail = -1
                ELSE: avail = 1
            ENDCASE
            END
        'mod2': BEGIN
            CASE var_name OF
                'hist2d_cot_ctp': avail = -1
                'hist1d_ctp'    : avail = -1
                'cth': avail = -1
                ELSE: avail = 1
            ENDCASE
            END
        'myd': BEGIN
            CASE var_name OF
                'hist2d_cot_ctp': avail = -1
                'hist1d_ctp'    : avail = -1
                'cth': avail = -1
                ELSE: avail = 1
            ENDCASE
            END
        'myd2': BEGIN
            CASE var_name OF
                'hist2d_cot_ctp': avail = -1
                'hist1d_ctp'    : avail = -1
                'cth': avail = -1
                ELSE: avail = 1
            ENDCASE
            END
        ELSE: avail = 1
    ENDCASE
    RETURN, avail
END


;-----------------------------------------------------------------------------
FUNCTION GET_DATE_UTC, filename
;-----------------------------------------------------------------------------
    IF (STRPOS(filename,'.nc') GE 0) THEN BEGIN
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
FUNCTION GET_FILE_NAME, filename, varname, type, scops_type, flag, mpc, $ 
                       CONSTANT_CER=creff, RATIO=ratio
;-----------------------------------------------------------------------------
    IF (scops_type EQ 1) THEN st = 'random' ELSE st='max/random'
    ststr = STRTRIM(STRING(scops_type),2)
    mpstr = STRTRIM(STRING(mpc),2)
    basen = FSC_Base_Filename(filename)
    obase = !SAVE_DIR + basen + '_' + flag + '_scops'+ststr + '_mpc'+mpstr
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
            title = 'Cloud Top Pressure [hPa]'
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


;-----------------------------------------------------------------------------
PRO ADD_SUBPLOT, xaxis, yaxis, ymin, ymax, xtitle=xtitle, ytitle=ytitle
;-----------------------------------------------------------------------------
    PLOT, xaxis, yaxis, YRANGE=[ymin,ymax], CHARSIZE=4.8, $
        XRANGE=[xaxis[0]-1, xaxis[N_ELEMENTS(xaxis)-1]+1],$
        COLOR=cgcolor('black'), YTITLE=ytitle, XTITLE=xtitle, $
        /NODATA, /XSTYLE, /YSTYLE;, XTICKINTERVAL=3, XMINOR=3
    OPLOT, yaxis, COLOR=cgcolor('Blue'), SYMSIZE=2.3, PSYM=cgsymcat(46)
END


;-----------------------------------------------------------------------------
PRO PLOT_CLOUD_ARRAYS, ctp, cth, ctt, cph, cer, cfc, cot, cwp, $
                       ncol, inp, cnt, sza, xi, yi, scops, mpc, thv
;-----------------------------------------------------------------------------
    t = '_thv'+STRTRIM(STRING(thv, FORMAT='(F4.2)'),2)
    s = '_scops'+STRTRIM(STRING(scops),2)
    m = '_mpc'+STRTRIM(STRING(mpc),2)
    p = '_retrieved'
    v = STRTRIM(STRING(cnt),2)
    fbase = inp.FILENAME + t + s + m + p + v

    IF (is_file(!SAVE_DIR+fbase+'.png')) THEN RETURN
    IF (is_file(!SAVE_DIR+fbase+'.eps')) THEN RETURN

    !P.MULTI=[0,4,2]
    save_as = !SAVE_DIR  + fbase + '.eps'
    start_save, save_as, size=[45,30]

    lonstr = STRTRIM(STRING(inp.lon[xi], FORMAT='(F8.1)'),2)
    latstr = STRTRIM(STRING(inp.lat[yi], FORMAT='(F8.1)'),2)
    szastr = STRTRIM(STRING(sza[xi,yi], FORMAT='(F8.1)'),2)
    pixel = 'Grid box: LON='+lonstr+'  LAT='+latstr+'  SZA='+szastr
    xaxis = FINDGEN(ncol)
    xt = 'Subcolumn'

    ADD_SUBPLOT, xaxis, cfc, -0.5, 1.5, ytitle='CFC', xtitle=xt
    ADD_SUBPLOT, xaxis, ctp, -100, 1000, ytitle='CTP [hPa]', xtitle=xt
    ADD_SUBPLOT, xaxis, ctt, -15, 350, ytitle='CTT [K]', xtitle=xt
    ADD_SUBPLOT, xaxis, cth/1000., -1, 15, ytitle='CTH [km]', xtitle=xt
    ADD_SUBPLOT, xaxis, cph, -1.5, 1.5, ytitle='CPH', xtitle=xt
    ADD_SUBPLOT, xaxis, cer, -10, 100., ytitle='CER [microns]', xtitle=xt
    ADD_SUBPLOT, xaxis, cot, -10, 110, ytitle='COT', xtitle=xt
    ADD_SUBPLOT, xaxis, cwp, -1.5, 1.5, ytitle='CWP [kg/m!U2!N]', xtitle=xt

    cgText, 0.5, 0.485, pixel, ALIGNMENT=0.5, /NORMAL, $
        COLOR=cgcolor('RED6'), CHARSIZE=2.8

    end_save, save_as
    !P.MULTI = 0
END


;-----------------------------------------------------------------------------
PRO MAKE_SCOPS_SNAPSHOTS, inp, grd, sza, xi, yi, data, target, $
                          scops, mpc, thv, cnt, do_profile=do_profile
;-----------------------------------------------------------------------------
    t = '_thv'+STRTRIM(STRING(thv, FORMAT='(F4.2)'),2)
    s = '_scops'+STRTRIM(STRING(scops),2)
    m = '_mpc'+STRTRIM(STRING(mpc),2)
    p = '_'+STRJOIN(STRSPLIT(target, /EXTRACT), '_')
    v = STRTRIM(STRING(cnt),2)
    fbase = inp.FILENAME + t + s + m + p + v
    save_as = !SAVE_DIR  + fbase + '.eps'

    IF (is_file(!SAVE_DIR+fbase+'.png')) THEN RETURN
    IF (is_file(!SAVE_DIR+fbase+'.eps')) THEN RETURN

    start_save, save_as, size='A4', /LANDSCAPE

    lonstr = STRTRIM(STRING(inp.lon[xi], FORMAT='(F8.1)'),2)
    latstr = STRTRIM(STRING(inp.lat[yi], FORMAT='(F8.1)'),2)
    szastr = STRTRIM(STRING(sza[xi,yi], FORMAT='(F8.1)'),2)
    pixel = 'Grid box: LON='+lonstr+'  LAT='+latstr+'  SZA='+szastr
    ytick = REVERSE(inp.PLEVEL/100.)
    ytstr = FLTARR(ROUND(N_ELEMENTS(ytick)/2.))
    j = 0
    FOR i=0, N_ELEMENTS(ytick)-1 DO BEGIN
        IF ((i MOD 2) EQ 0) THEN BEGIN
            ytstr[j] = ytick[i] & j++
        ENDIF ELSE BEGIN 
            CONTINUE
        ENDELSE
    ENDFOR

    ytickname = STRCOMPRESS(STRING(ytstr,FORMAT='(I4)'),/rem)
    bar_nlev = 6
    col_table = 2
    void = WHERE(data LE 0.)
    bar_format = '(f5.1)'

    CASE target OF
        'CFC': BEGIN
            title = pixel
            col_table = -10 
            bar_title = 'Cloud Fraction'
            void = WHERE(data LT 0.)
            END
        'CPH': BEGIN
            bar_title = 'Cloud Phase'
            void = WHERE(data LT 0.)
            END
        'COT': BEGIN
            bar_title = 'Cloud Optical Thickness'
            END
        'CWP': BEGIN
            bar_title = 'Cloud Water Path [kg/m!U2!N]'
            bar_format = '(f5.2)'
            END
        'CER': bar_title = 'Cloud Effective Radius [microns]'
        ELSE: BEGIN
            bar_title = ''
            title = ''
            END
    ENDCASE

    view2d, data, col_table=col_table, /color, chars=2.8, $
        bar_title=bar_title, xtitle='Subcolumn', /xstyle, /ystyle, $
        ytitle='Pressure Level [hPa]', ytickname=ytickname, $
        yticks=N_ELEMENTS(ytickname)-1, no_data_idx=void, $
        bar_tickname=bar_tickname, bar_nlev=bar_nlev, bar_format=bar_format

    IF (target EQ 'CFC') THEN $ 
        cgText, 0.65, 0.91, title, ALIGNMENT=0.5, /NORMAL, $
            COLOR='RED6', CHARSIZE=2.8

    end_save, save_as

    ; plot profile
    IF KEYWORD_SET(do_profile) THEN BEGIN
        save_as2 = !SAVE_DIR  + fbase + '_profile.eps'
        start_save, save_as2, size='A4', /LANDSCAPE

        plev_prof = inp.PLEVEL[0:N_ELEMENTS(inp.PLEVEL)-2]*0.5 + $
                    inp.PLEVEL[1:N_ELEMENTS(inp.PLEVEL)-1]*0.5
        yplev = REVERSE(plev_prof/100.)

        PLOT, [0,0], [1,1], YRANGE=[1000,100], $
            XRANGE=[MIN(do_profile), MAX(do_profile)], $
            COLOR=cgcolor('black'), XTITLE=bar_title, $
            YTITLE='Pressure Level [hPa]', CHARS=3., $
            YTICKINTERVAL=100, YMINOR=2
        OPLOT, do_profile, yplev, psym=-1, symsize=2.5, $
            COLOR=cgcolor('Blue'), THICK=5
        cgText, 0.55, 0.95, pixel, ALIGNMENT=0.5, /NORMAL, $
            COLOR=cgcolor('RED6'), CHARSIZE=3

        end_save, save_as2
    ENDIF
END


;-----------------------------------------------------------------------------
PRO PLOT_ERA_SST, FILENAME=filename, DATA=sst, $
                  LATITUDE=lat, LONGITUDE=lon, VOID=void_index
;-----------------------------------------------------------------------------
    !EXCEPT=0

    filepwd = !SAVE_DIR + filename

    IF ( is_file(filepwd+'.png') ) THEN RETURN
    IF ( is_file(filepwd+'.eps') ) THEN RETURN

    save_as = filepwd + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE

    limit = [-90., -180., 90., 180.]

    MAP_IMAGE, sst, lat, lon, LIMIT=limit, $
               CTABLE=33, /BOX_AXES, /MAGNIFY, /GRID, $
               FORMAT=('(f5.1)'), N_LEV=6, $
               MINI=MIN(sst), MAXI=MAX(sst), $
               CHARSIZE=3., VOID_INDEX=void_index, $
               ;FIGURE_TITLE="ERA-Interim Sea Surface Temperature", $
               TITLE='SST [K]'

    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2

    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2

    end_save, save_as

END


;-----------------------------------------------------------------------------
PRO PLOT_LSM2D, FILENAME=filename, DATA=lsm, $
                LATITUDE=lat, LONGITUDE=lon, TITLE=title
;-----------------------------------------------------------------------------
    !EXCEPT=0

    filepwd = !SAVE_DIR + filename

    IF ( is_file(filepwd+'.png') ) THEN RETURN
    IF ( is_file(filepwd+'.eps') ) THEN RETURN

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
               /NO_COLOR_BAR, $
               ;FIGURE_TITLE=title, $
               CHARSIZE=3., VOID_INDEX=void_index

    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2

    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2

    end_save, save_as

END


;-----------------------------------------------------------------------------
PRO PLOT_SZA2D, FILENAME=filename, DATA=sza2d, $
                LATITUDE=lat, LONGITUDE=lon, TITLE=title, DIFF=diff
;-----------------------------------------------------------------------------
    !EXCEPT=0

    filepwd = !SAVE_DIR + filename + '_sza'

    IF ( is_file(filepwd+'.png') ) THEN RETURN
    IF ( is_file(filepwd+'.eps') ) THEN RETURN

    save_as = filepwd + '.eps'
    start_save, save_as, size='A4', /LANDSCAPE
    limit = [-90., -180., 90., 180.]

    IF KEYWORD_SET(diff) THEN BEGIN
        MAP_IMAGE, sza2d, lat, lon, LIMIT=limit, $
                   CTABLE=70, /BOX_AXES, /MAGNIFY, /GRID, $
                   MINI=MIN(sza2d), MAXI=MAX(sza2d), $
                   CHARSIZE=2.5, TITLE=title, N_LEV=6, FORMAT=('(f7.3)') 
    ENDIF ELSE BEGIN
        MAP_IMAGE, sza2d, lat, lon, LIMIT=limit, $
                   CTABLE=33, /FLIP_COLOURS, $
                   /BOX_AXES, /MAGNIFY, /GRID, $
                   MINI=0., MAXI=180., CHARSIZE=3., $
                   TITLE=title, N_LEV=6
    ENDELSE

    MAP_CONTINENTS, /CONTINENTS, /HIRES, $
        COLOR=cgcolor('Black'), GLINETHICK=2.2

    MAP_GRID, COLOR=cgcolor('Black'), MLINETHICK=2.2

    end_save, save_as

END


;-----------------------------------------------------------------------------
PRO MAP_MM, grd, data, filename, varname, flag, scops_type, mpc, $
            TITLE=title, CONSTANT_CER=creff
;-----------------------------------------------------------------------------
    !EXCEPT=0

    ofil = GET_FILE_NAME( filename, varname, 'map', scops_type, flag, mpc, $
                         CONSTANT_CER=creff )

    IF (is_file(ofil+'.png')) THEN RETURN
    IF (is_file(ofil+'.eps')) THEN RETURN

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

END


;-----------------------------------------------------------------------------
PRO PLOT_HISTOS_1D, varname, data, histo, filename, flag, scops_type, mpc, $
                    CONSTANT_CER=creff, RATIO=ratio
;-----------------------------------------------------------------------------

    ofil = GET_FILE_NAME( filename, varname, 'hist1d', scops_type, flag, $
                          mpc, CONSTANT_CER=creff, RATIO=ratio )

    IF (is_file(ofil+'.png')) THEN RETURN
    IF (is_file(ofil+'.eps')) THEN RETURN

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

END


;-----------------------------------------------------------------------------
PRO PLOT_INTER_HISTOS, data, varname, histo, filename, flag, scops_type, $
                       mpc, CONSTANT_CER=creff, RATIO=ratio
;-----------------------------------------------------------------------------

    ofil = GET_FILE_NAME( filename, varname, '2Dtmp', scops_type, flag, mpc, $
                         CONSTANT_CER=creff, RATIO=ratio )

    IF ( is_file( ofil + '.png' ) ) THEN RETURN
    IF ( is_file( ofil + '.eps' ) ) THEN RETURN

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
    
    cgText, 0.52, 0.49, Alignment=0.5, /Normal, 'SCOPS.type='+st, chars=cs

    ; end plotting
    end_save, save_as
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
    IF ( is_file(filepwd+'.eps') ) THEN RETURN

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

    !P.MULTI = 0
END


;-----------------------------------------------------------------------------
PRO CREATE_1DHIST, RESULT=res, VARNAME=vn, VARSTRING=vs, $
                   CHARSIZE=cs, XTITLE=xtitle, YMAX=ymax, $
                   LEGEND_POSITION=lp, RATIO=ratio, BIN_BORDERS=xtickname,$
                   COMPARE=compare, FILE=file
;-----------------------------------------------------------------------------
    IF NOT KEYWORD_SET(ymax) THEN ymax = 40
    IF NOT KEYWORD_SET(lp) THEN lp='tl'
    IF KEYWORD_SET(ratio) THEN ymax = 100

    IF KEYWORD_SET(compare) THEN BEGIN
        ; get reference data
        ref_liq = GET_DATA(compare.YEAR, compare.MONTH, sat=compare.SAT, $
                           algo=compare.REF, level='l3c', $
                           dirname=compare.DIRNAME, $
                           data='hist1d_' + compare.VAR + '_liq', /silent) 
        ref_ice = GET_DATA(compare.YEAR, compare.MONTH, sat=compare.SAT, $
                           algo=compare.REF, level='l3c', $
                           dirname=compare.DIRNAME, $
                           data='hist1d_' + compare.VAR + '_ice', /silent) 
        ; consider fill_values
        ref_all = ( ref_liq>0 ) + ( ref_ice>0 )

        IF (compare.VAR EQ 'cer') THEN dname = 'ref' ELSE dname = compare.VAR

        ; prepare plotting results
        refbild = get_1d_rel_hist_from_1d_hist( ref_all, $
                    'hist1d_'+dname, algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic, $
                    ytitle = ytitle, hist_name=data_name, found=found1)

        refbild1 = get_1d_rel_hist_from_1d_hist( ref_liq, $
                    'hist1d_'+dname+'_liq', algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                    ytitle = ytitle, hist_name=data_name, found=found1)

        refbild2 = get_1d_rel_hist_from_1d_hist( ref_ice, $
                    'hist1d_'+dname+'_ice', algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                    ytitle = ytitle, hist_name=data_name, found=found1)

        IF KEYWORD_SET(ratio) THEN BEGIN
            ref_total = GET_DATA(compare.YEAR, compare.MONTH, sat=compare.SAT, $
                                 algo=compare.REF, level='l3c', $
                                 dirname=compare.DIRNAME, $
                                 data='hist1d_' + compare.VAR, /silent) 
            refbild3 = get_1d_rel_hist_from_1d_hist( ref_total, $
                    'hist1d_'+dname+'_ratio', algo=compare.REF, $
                    land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                    ytitle = ytitle, hist_name=data_name, found=found1)
        ENDIF
    ENDIF


    bild_liq = reform(res[*,*,*,0])
    bild_ice = reform(res[*,*,*,1])
    bild_all = ( bild_liq>0 ) + ( bild_ice>0 )

    IF (vn EQ 'cer') THEN dname = 'ref' ELSE dname = vn

    bild = get_1d_rel_hist_from_1d_hist( bild_all, 'hist1d_'+dname, $
                algo='era-i', land=land, sea=sea, arctic=arctic, $
                antarctic=antarctic, ytitle = ytitle, hist_name=data_name, $
                found=found1, file=file)

    bild1 = get_1d_rel_hist_from_1d_hist( bild_liq, $
                'hist1d_'+dname+'_liq', algo='era-i', $
                land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                ytitle = ytitle, hist_name=data_name, $
                found=found1, file=file)

    bild2 = get_1d_rel_hist_from_1d_hist( bild_ice, $
                'hist1d_'+dname+'_ice', algo='era-i', $
                land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                ytitle = ytitle, hist_name=data_name, $
                found=found1, file=file)

    IF KEYWORD_SET(ratio) THEN BEGIN
        bild3 = get_1d_rel_hist_from_1d_hist( res, $
                'hist1d_'+dname+'_ratio', algo='era-i', $
                land=land, sea=sea, arctic=arctic, antarctic=antarctic,$ 
                ytitle = ytitle, hist_name=data_name, $
                found=found1, file=file)
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
                   cci_pwd, SAT=sat, REFS=refs, RATIO=ratio
;-----------------------------------------------------------------------------
    CASE varname OF
        'cer': ymax=50.
        'cot': ymax=30.
        'cwp': ymax=30.
        'ctt': ymax=50.
        'ctp': ymax=40.
        ELSE: RETURN
    ENDCASE
    
    opt = 'hist1d_'
    outfile = save_dir + base + '_' + opt + varname 
    IF KEYWORD_SET(ratio) THEN outfile = outfile + '_ratio'
    
    ;READ_SIM_NCDF, h1d, FILE=file, VAR_NAME=opt+varname
    READ_SIM_NCDF, bin, FILE=file, VAR_NAME=opt+varname+'_bin_border'
    READ_NCDF, file, opt+varname, verbose = verbose, found = found, algoname = 'era-i', set_fillvalue = set_fillvalue , $	;input 
	h1d, fillvalue, minvalue, maxvalue, longname, unit, raw=raw, attribute = attribute, var_dim_names = var_dim_names	;output
    
    save_as = outfile + '.eps'
    start_save, save_as, size=[35,20]
    cs = 2.3
    varn = varname

    CREATE_1DHIST, RESULT=h1d, VARNAME=varn, $
        VARSTRING=opt+varn, CHARSIZE=cs, XTITLE=xtitle, $
        YMAX=ymax, LEGEND_POSITION=legend_position, RATIO=ratio, $
        BIN_BORDERS=bin, FILE=file
    
    end_save, save_as
    
    IF KEYWORD_SET(refs) THEN BEGIN
        FOR r=0, N_ELEMENTS(refs)-1 DO BEGIN

            is = GET_AVAILABILITY(opt+varname, refs[r])
            IF (is EQ -1) THEN CONTINUE

            CASE varname of
                'cer' : BEGIN
                    IF ((STRLEN(cci_pwd) GT 0) AND (refs[r] EQ 'cci')) THEN $
                        varn='cer' ELSE varn='ref'
                    END
                ELSE: varn = varname 
            ENDCASE

            CASE refs[r] OF
                'cci': dirn = cci_pwd
                ELSE: dirn = ''
            ENDCASE

            opt = 'hist1d_'
            outfile = save_dir + base + '_' + opt + varname
            IF KEYWORD_SET(ratio) THEN outfile = outfile + '_ratio'
            outfile = outfile + '_compare_with_'+refs[r]
    
            save_as = outfile + '.eps'
            start_save, save_as, size=[35,20]
            cs = 2.3
    
            compare = {ref:refs[r], sat:sat, var:varn, dirname:dirn, $
                       year:STRMID(time, 0, 4), $
                       month:STRMID(time, 4, 2), dat:''}
    
            CREATE_1DHIST, RESULT=h1d, VARNAME=varn, $
                VARSTRING=opt+varn, CHARSIZE=cs, XTITLE=xtitle, $
                YMAX=ymax, LEGEND_POSITION=legend_position, RATIO=ratio, $
                BIN_BORDERS=bin, COMPARE=compare, FILE=file
    
            end_save, save_as
        ENDFOR
    ENDIF

END


;-----------------------------------------------------------------------------
PRO PLOT_SIM_MAPS, file, varname, save_dir, data, fillvalue, mini, maxi, $
                   base, figure_title, xtitle
;-----------------------------------------------------------------------------
    IF (varname EQ 'hist2d_cot_ctp') THEN RETURN

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
                           mini, maxi, cci_pwd, SAT=sat
;-----------------------------------------------------------------------------
    IF (varname EQ 'hist2d_cot_ctp') THEN RETURN

    FOR r=0, N_ELEMENTS(refs)-1 DO BEGIN
    
        is = GET_AVAILABILITY(varname, refs[r])
        IF (is EQ -1) THEN CONTINUE

        year = STRMID(time, 0, 4)
        month = STRMID(time, 4, 2) 

        CASE varname of
            'cer' : BEGIN
                IF ((STRLEN(cci_pwd) GT 0) AND (refs[r] EQ 'cci')) THEN $
                    cci_varn=varname ELSE cci_varn='ref'
                END
            'cer_liq' : BEGIN
                IF ((STRLEN(cci_pwd) GT 0) AND (refs[r] EQ 'cci')) THEN $
                    cci_varn=varname ELSE cci_varn='ref_liq'
                END
            'cer_ice' : BEGIN
                IF ((STRLEN(cci_pwd) GT 0) AND (refs[r] EQ 'cci')) THEN $
                    cci_varn=varname ELSE cci_varn='ref_ice'
                END
            ELSE: cci_varn = varname 
        ENDCASE

        CASE refs[r] OF
            'cci': dirn = cci_pwd
            ELSE: dirn = ''
        ENDCASE

        compare_cci_with_clara, year, month, '', ALGO1='era-i',$
            DATA=cci_varn, CCIFILE=file, REFERENCE=refs[r], $
            SAT=sat, MINI=mini, MAXI=maxi , LIMIT=limit, $
            SAVE_DIR=save_dir, LAND=land, SEA=sea, COV=cov, $
            OTHER='rainbow', CTABLE='', LEVEL='l3c', DIRNAME2=dirn

    ENDFOR
END


;-----------------------------------------------------------------------------
PRO PLOT_SIM_COMPARE_ZONAL, file, vname, time, save_dir, base, $
                            mini, maxi, cci_pwd, REFS=refs, SAT=sat
;-----------------------------------------------------------------------------
    IF (vname EQ 'hist2d_cot_ctp') THEN RETURN

    opt = 'zonal_'
    outfile = save_dir + base + '_' + opt + vname
    save_as = outfile + '.eps'
    start_save, save_as, size=[32,20]

    year = STRMID(time, 0, 4)
    month = STRMID(time, 4, 2) 
    datum = year + month
    
    CASE vname OF
        'cfc' : maxv=maxi+maxi/10.
        'cph' : maxv=maxi+maxi/10.
        ELSE: maxv=maxi
    ENDCASE
    
    opl = 0
    plot_zonal_average, year, month, '', $
        file, vname, algo='era-i', sea=sea, land=land, $
        limit=limit, mini=mini, maxi=maxv, $
        found=found, level=level, datum=datum, error=error, $
        oplots=opl, addtext='Simulated', /simulator
    
    IF KEYWORD_SET(refs) THEN BEGIN
        FOR r=0, N_ELEMENTS(refs)-1 DO BEGIN

            is = GET_AVAILABILITY(vname, refs[r])
            IF (is EQ -1) THEN CONTINUE

            IF ((STRLEN(cci_pwd) EQ 0) OR (refs[r] NE 'cci')) THEN BEGIN
                CASE vname OF
                    'cer':      varname = 'ref'
                    'cer_liq':  varname = 'ref_liq'
                    'cer_ice':  varname = 'ref_ice'
                    ELSE: varname = vname
                ENDCASE
            ENDIF ELSE BEGIN
                varname = vname
            ENDELSE

            CASE refs[r] OF
                'cci': dirn = cci_pwd 
                ELSE: dirn = ''
            ENDCASE

            plot_zonal_average, year, month, '', $
                '', varname, algo=refs[r], sea=sea, land=land, $
                limit=limit, satellite=sat, $
                found=found, level=level, datum=datum, error=error, $
                oplots=r+opl+1, /simulator, dirname=dirn

        ENDFOR
    ENDIF

    end_save, save_as

END



;-----------------------------------------------------------------------------
PRO PLOT_SIM_COMPARE_JCH, file, vname, time, save_dir, base, $
                          mini, maxi, fillvalue, cci_pwd, $
                          REFS=refs, SAT=sat
;-----------------------------------------------------------------------------
    CASE vname OF
        'hist2d_cot_ctp': opt = '_hist2d' 
        ELSE: RETURN
    ENDCASE

    set_colors, other='rainbow', ctable=ctable, $
                brewer=brewer, col_tab=col_tab

    year = STRMID(time, 0, 4)
    month = STRMID(time, 4, 2)
    outfile = save_dir + base + opt

    ; read pseudo-satellite data
    READ_SIM_NCDF, h2d, FILE=file, VAR_NAME=vname
    data_liq = REFORM(h2d[*,*,*,*,0])
    data_ice = REFORM(h2d[*,*,*,*,1])
    data_all = ( data_liq>0 ) + ( data_ice>0 )

    ; get data name
    era_alg = sat_name('era-i','', year=year, month=month)

    ; get relative occurrences
    era_liq = get_2d_rel_hist_from_jch( data_liq, 'era-i', $ 
        dem=dem, land=land, sea=sea, limit=limit, $ 
        antarctic=antarctic, arctic=arctic, $ 
        lon=lon, lat=lat, fillvalue=fillvalue, found=found )

    era_ice = get_2d_rel_hist_from_jch( data_ice, 'era-i', $ 
        dem=dem, land=land, sea=sea, limit=limit, $ 
        antarctic=antarctic, arctic=arctic, $ 
        lon=lon, lat=lat, fillvalue=fillvalue, found=found )

    era_all = get_2d_rel_hist_from_jch( data_all, 'era-i', $ 
        dem=dem, land=land, sea=sea, limit=limit, $ 
        antarctic=antarctic, arctic=arctic, $ 
        lon=lon, lat=lat, fillvalue=fillvalue, found=found )


    ; sim-plots
    plot_2d_rel_hist, era_liq, era_alg+' (liquid)', $ 
        col_tab=col_tab, brewer=brewer, $
        mini=mini, maxi=maxi, save_as=outfile+'_liq.eps'

    plot_2d_rel_hist, era_ice, era_alg+' (ice)', $ 
        col_tab=col_tab, brewer=brewer, $
        mini=mini, maxi=maxi, save_as=outfile+'_ice.eps'

    plot_2d_rel_hist, era_all, era_alg, $ 
        col_tab=col_tab, brewer=brewer, $
        mini=mini, maxi=maxi, save_as=outfile+'.eps'


    IF KEYWORD_SET(refs) THEN BEGIN
        FOR r=0, N_ELEMENTS(refs)-1 DO BEGIN

            is = GET_AVAILABILITY(vname, refs[r])
            IF (is EQ -1) THEN CONTINUE

            CASE refs[r] OF
                'cci': dirn = cci_pwd
                ELSE: dirn = ''
            ENDCASE

            ; get reference data name
            ref_alg = sat_name(refs[r], sat, year=year, month=month)

            ; get data
            ref_liq = GET_DATA( year, month, sat=sat, algo=refs[r], $ 
                dirname=dirn, /silent, $
                nan_fillv=fillvalue, level='l3c', data=vname+'_liq' )
            ref_ice = GET_DATA( year, month, sat=sat, algo=refs[r], $ 
                dirname=dirn, /silent, $
                nan_fillv=fillvalue, level='l3c', data=vname+'_ice' )
            ref_all = ( ref_liq>0 ) + ( ref_ice>0 )

            ; get relative occurrences
            ref_liq_bild = get_2d_rel_hist_from_jch( ref_liq, refs[r], $ 
                dem=dem, land=land, sea=sea, limit=limit, $ 
                antarctic=antarctic, arctic=arctic, $ 
                lon=lon, lat=lat, fillvalue=fillvalue, found=found )

            ref_ice_bild = get_2d_rel_hist_from_jch( ref_ice, refs[r], $ 
                dem=dem, land=land, sea=sea, limit=limit, $ 
                antarctic=antarctic, arctic=arctic, $ 
                lon=lon, lat=lat, fillvalue=fillvalue, found=found )

            ref_all_bild = get_2d_rel_hist_from_jch( ref_all, refs[r], $ 
                dem=dem, land=land, sea=sea, limit=limit, $ 
                antarctic=antarctic, arctic=arctic, $ 
                lon=lon, lat=lat, fillvalue=fillvalue, found=found )

            ; ref-plots
            plot_2d_rel_hist, ref_liq_bild, ref_alg+' (liquid)', $ 
                col_tab=col_tab, brewer=brewer, mini=mini, maxi=maxi, $
                save_as=outfile+'_liq_'+refs[r]+'.eps'

            plot_2d_rel_hist, ref_ice_bild, ref_alg+' (ice)', $ 
                col_tab=col_tab, brewer=brewer, mini=mini, maxi=maxi, $
                save_as=outfile+'_ice_'+refs[r]+'.eps'

            plot_2d_rel_hist, ref_all_bild, ref_alg, $
                col_tab=col_tab, brewer=brewer, mini=mini, maxi=maxi, $
                save_as=outfile+'_'+refs[r]+'.eps'

            ; diff-plots
            plot_2d_rel_hist, era_all, era_alg, $
                bild2=ref_all_bild, name2=ref_alg, /difference, $ 
                col_tab=4, brewer=brewer, mini=-maxi, maxi=maxi, $ 
                save_as=save_dir+base+'_diff_sim-'+refs[r]+'.eps'

            plot_2d_rel_hist, era_ice, era_alg, $
                bild2=ref_ice_bild, name2=ref_alg+' (ice)', /difference, $
                col_tab=4, brewer=brewer, mini=-maxi, maxi=maxi, $ 
                save_as=save_dir+base+'_diff_sim-'+refs[r]+'_ice.eps'

            plot_2d_rel_hist, era_liq, era_alg, $
                bild2=ref_liq_bild, name2=ref_alg+' (liq)', /difference, $
                col_tab=4, brewer=brewer, mini=-maxi, maxi=maxi, $ 
                save_as=save_dir+base+'_diff_sim-'+refs[r]+'_liq.eps'

        ENDFOR
    ENDIF
END


;-----------------------------------------------------------------------------
PRO PLOT_PROFILES, file, varname, save_dir
;-----------------------------------------------------------------------------

    fbase = FSC_Base_Filename(file)
    save_as = save_dir  + fbase + '_' + varname + '.eps'

    READ_SIM_NCDF, p, FILE=file, VAR_NAME='pressure_profile', GLOB_ATTR=globs
    READ_SIM_NCDF, v, FILE=file, VAR_NAME = varname

    pressure = REVERSE(p)
    data = REVERSE(v)

    ;** Structure <402ee08>, 11 tags, length=176, data length=176, refs=1:
    ;LON             STRING    '      71.5000'
    ;LAT             STRING    '      26.0000'
    ;SZA             STRING    '      17.9144'
    ;COT_THV         STRING    '     0.150000'
    ;SCOPS           STRING    'dwd_scops'
    ;OVERLAP         STRING    'overlap_maxrand'
    ;MPC             STRING    'mixed_phase'
    ;START_DATE      STRING    '20080701'
    ;END_DATE        STRING    '20080701'
    ;HOUR            STRING    '       6'
    ;FILENAME        STRING    'ERA_Interim_an_20080701_0600+00_mlev.nc'

    lonstr = STRTRIM(STRING(globs.lon, FORMAT='(F6.2)'),2)
    latstr = STRTRIM(STRING(globs.lat, FORMAT='(F6.2)'),2)
    szastr = STRTRIM(STRING(globs.sza, FORMAT='(F6.2)'),2)
    pixel = 'Grid Box: Longitude = '+lonstr+$
            '  Latitude = '+latstr+$
            '  Solar Zenith Angle = '+szastr

    !P.MULTI=[0,1,1]

    position = [0.13, 0.13, 0.95, 0.9]
    chars = 2.7
    syms = 2.0
    yrange = [1000., 100.]
    ytickv = [1000., 800., 600., 400., 200., 100.]
    yticks = N_ELEMENTS(ytickv) - 1
    yminor = 2

    start_save, save_as, size='A4', /LANDSCAPE

    ; plot profile
    CASE varname OF
        'cfc_profile': xtitle = 'Cloud Fraction'
        'cot_profile': xtitle = 'Cloud Optical Thickness'
        'cwp_profile': xtitle = 'Cloud Water Path [kg/m!U2!N]'
        'cer_profile': xtitle = 'Cloud Effective Radius [micron]'
        ELSE: BEGIN
            print, " --- ", varname, " is not defined! "
            stop
            END
    ENDCASE

    PLOT, [0,0], [1,1], YRANGE=yrange, /YLOG, POSITION=position, $
        YTICKV=ytickv, YTICKS=yticks, YMINOR=yminor, $
        XRANGE=[MIN(data), MAX(data)], $
        YTITLE='Pressure Level [hPa]', CHARS=chars, $
        COLOR=cgcolor('black'), XTITLE=xtitle

    OPLOT, data, pressure, psym=-1, symsize=syms, $
        COLOR=cgcolor('Blue'), THICK=5

    IF ( varname EQ 'cfc_profile' ) THEN BEGIN 
        cgText, 0.55, 0.94, pixel, ALIGNMENT=0.52, /NORMAL, $ 
            COLOR=cgcolor('RED6'), CHARSIZE=chars
    ENDIF

    end_save, save_as

    !P.MULTI = 0

END

;-----------------------------------------------------------------------------
PRO PLOT_MATRICES, file, varname, save_dir
;-----------------------------------------------------------------------------

    fbase = FSC_Base_Filename(file)
    save_as = save_dir  + fbase + '_' + varname + '.eps'

    READ_SIM_NCDF, pressure, FILE=file, VAR_NAME='pressure_profile', GLOB_ATTR=globs
    READ_SIM_NCDF, var, FILE=file, VAR_NAME = varname 

    data = ROTATE(var,7)
    dims = SIZE(var)
    cols = dims[1]
    levs = dims[2]

    ;** Structure <402ee08>, 11 tags, length=176, data length=176, refs=1:
    ;LON             STRING    '      71.5000'
    ;LAT             STRING    '      26.0000'
    ;SZA             STRING    '      17.9144'
    ;COT_THV         STRING    '     0.150000'
    ;SCOPS           STRING    'dwd_scops'
    ;OVERLAP         STRING    'overlap_maxrand'
    ;MPC             STRING    'mixed_phase'
    ;START_DATE      STRING    '20080701'
    ;END_DATE        STRING    '20080701'
    ;HOUR            STRING    '       6'
    ;FILENAME        STRING    'ERA_Interim_an_20080701_0600+00_mlev.nc'

    lonstr = STRTRIM(STRING(globs.lon, FORMAT='(F6.2)'),2)
    latstr = STRTRIM(STRING(globs.lat, FORMAT='(F6.2)'),2)
    szastr = STRTRIM(STRING(globs.sza, FORMAT='(F6.2)'),2)
    pixel = 'Grid Box: Longitude = '+lonstr+$
            '  Latitude = '+latstr+$
            '  Solar Zenith Angle = '+szastr

    !P.MULTI=[0,1,1]

    chars = 2.5
    syms = 2.0

    start_save, save_as, size='A4', /LANDSCAPE

    bar_nlev = 6
    col_table = 2
    void = WHERE(data LE 0.)
    bar_format = '(f5.1)'
    flag = 0

    CASE varname OF
        'cfc_matrix': BEGIN
            title = pixel
            col_table = -10 
            bar_title = 'Cloud Fraction'
            void = WHERE(data LT 0.)
            flag = 1
            END
        'cph_matrix': BEGIN
            bar_title = 'Cloud Phase'
            void = WHERE(data LT 0.)
            END
        'cot_matrix': BEGIN
            bar_title = 'Cloud Optical Thickness'
            END
        'cwp_matrix': BEGIN
            bar_title = 'Cloud Water Path [kg/m!U2!N]'
            bar_format = '(f5.2)'
            END
        'cer_matrix': bar_title = 'Cloud Effective Radius [microns]'
        ELSE: BEGIN
            print, " --- ", varname, " is not defined! "
            stop
            END
    ENDCASE

    inter = 5
    ytick = REVERSE(pressure)
    ytstr = FLTARR(levs/inter +1)
    j = 0
    FOR i=0, N_ELEMENTS(ytick)-1 DO BEGIN
        ;IF ((i MOD 10) EQ 0) THEN BEGIN
        IF ((i MOD inter) EQ 0) THEN BEGIN
            ytstr[j] = ytick[i] & j++
        ENDIF ELSE BEGIN 
            CONTINUE
        ENDELSE
    ENDFOR

    yminor = 2
    ytickname = STRCOMPRESS(STRING(ytstr,FORMAT='(F8.1)'),/rem)

    view2d, data, POSITION=[0., 0., 0.93, 0.95], $
        YMINOR=yminor, $
        ytickname=ytickname, yticks=N_ELEMENTS(ytickname)-1, $
        COL_TABLE=col_table, /COLOR, CHARS=chars, $
        BAR_TITLE=bar_title, XTITLE='Subcolumn', $
        YTITLE='Pressure Level [hPa]', NO_DATA_IDX=cfc_void, $
        BAR_NLEV=bar_nlev, BAR_FORMAT=bar_format, YSTYLE=8

    AXIS, YAXIS=1, YRANGE=[levs,0], $
        YMINOR=10, YTICKS=levs/10, $
        YTITLE='Model Level', CHARS=chars

    IF ( flag EQ 1 ) THEN BEGIN 
        cgText, 0.55, 0.95, pixel, ALIGNMENT=0.5, /NORMAL, $ 
            COLOR=cgcolor('RED6'), CHARSIZE=chars
    ENDIF

    end_save, save_as

    !P.MULTI = 0

END
