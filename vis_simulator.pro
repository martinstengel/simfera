@/../vali_gui/plot_l3.pro
@local_include_file.pro
;+
; NAME:
;   VIS_SIMULATOR
;
; PURPOSE:
;   Visualization tool for cloud_simulator output
;   including comparison with reference data
;
; AUTHOR:
;   Dr. Cornelia Schlundt
;   Deutscher Wetterdienst (DWD)
;   KU22, Climate-based satellite monitoring
;   cornelia.schlundt@dwd.de
;
; CATEGORY:
;   Graphics
;
; CALLING SEQUENCE:
;   vis_simulator, /map
;
; MODIFICATION HISTORY:
;   Jan 2016, first version
;
;******************************************************************************
PRO VIS_SIMULATOR, VERBOSE=verbose, FILE=file, INP=inp, OUT=out, RUN=run, $
                   HIST1D=hist1d, MAP=map, REF=ref, SAT=sat, $
                   MINI=mini, MAXI=maxi, $
                   YMAX=ymax, VARS=vars, RATIO=ratio, HELP=help
;******************************************************************************
    STT = SYSTIME(1)

    ; MODIFY settings
    IF KEYWORD_SET(run) THEN run = run ELSE $
        run = 'v5.1_advanced_scops/'
    IF KEYWORD_SET(inp) THEN inp = inp ELSE $
        inp = '/cmsaf/cmsaf-cld7/cschlund/output/simulator/'
    IF KEYWORD_SET(out) THEN out = out ELSE $
        out = '/cmsaf/cmsaf-cld7/cschlund/figures/simulator/'

    DEFSYSV, '!SAVE_DIR', out + run + 'VIS/'

    IF ~KEYWORD_SET(file) THEN BEGIN

        file = FINDFILE(inp+run+'*MM*nc',count=count)
        IF (count GT 0) THEN BEGIN 
            PRINT, '** ', STRTRIM(count,2), ' InputFiles found'
        ENDIF ELSE BEGIN 
            PRINT, '** NO FILES FOUND'
            RETURN
        ENDELSE
    ENDIF

    IF ~KEYWORD_SET(vars) THEN BEGIN 
        vars  = ['cer','cot','cwp','ctt','ctp','cfc','cph','cth',$
                 'lwp','iwp','cot_liq','cot_ice','cer_liq','cer_ice'] 
        IF KEYWORD_SET(hist1d) THEN vars = ['cer','cot','cwp','ctp','ctt']
    ENDIF


    IF KEYWORD_SET(help) THEN BEGIN
        PRINT, ""
        PRINT, " *** THIS PROGRAM VISUALIZES THE SIMULATOR OUTPUT AND",$
               " COMPARES IT WITH REFERENCE DATA ***"
        PRINT, ""
        PRINT, " USAGE: "
        PRINT, " VIS_SIMULATOR, /map"
        PRINT, " VIS_SIMULATOR, vars=['ctp'], /hist1d"
        PRINT, " VIS_SIMULATOR, vars=['cwp','cot'], /hist1d, ref='cci', sat='NOAA18' "
        PRINT, " VIS_SIMULATOR, vars=['cfc'], ref='cci', sat='NOAA18' "
        PRINT, ""
        PRINT, " Optional Keywords:"
        PRINT, " FILE           full qualified simulator ncfile, default is loop over file list"
        PRINT, " INP            path to input files for plotting, default is: ", inp
        PRINT, " OUT            path to output figures, default is: ", !SAVE_DIR
        PRINT, " RUN            subdirectory of input & output, default is: ", run
        PRINT, " VARS           list of parameters to be plotted, default is: ", vars
        PRINT, " HIST1D         creates 1D histogram plots"
        PRINT, " MAP            creates 2D maps"
        PRINT, " REF            compares simulator output with reference data, options: ", $
               " cci, gac, gac2, myd, mod, myd2, mod2, gwx, cal, isp, pmx, cla"
        PRINT, " SAT            REF requires SAT in some cases, e.g. sat='NOAA18'"
        PRINT, " VERBOSE        increase output verbosity."
        PRINT, " RATIO          adds liquid cloud fraction to HIST1D plot."
        PRINT, " YMAX           set max for y-axis for /HIST1D"
        PRINT, " MINI           set min value for map_image for /MAP"
        PRINT, " MAXI           set max value for map_image for /MAP"
        PRINT, " HELP           prints this message."
        PRINT, ""
        RETURN
    ENDIF


    FOR f=0, count-1 DO BEGIN ; loop over files

        PRINT, "** Working on: ", file[f]


        FOR i=0, N_ELEMENTS(vars)-1 DO BEGIN ; loop over variables


            ; read simulator output file
            READ_SIM_NCDF, data, FILE=file[f], VAR_NAME=vars[i], $ 
                GLOB_ATTR=gatt, VAR_ATTR=vatt

            source    = STRTRIM(STRING(gatt.SOURCE),2)
            time      = STRTRIM(STRING(gatt.TIME_COVERAGE_START),2)
            cot_thv   = STRTRIM(STRING(gatt.COT_THV,FORMAT='(F4.2)'),2)
            nfiles    = STRTRIM(STRING(gatt.NUMBER_OF_FILES),2)
            scops     = STRTRIM(STRING(gatt.SCOPS_TYPE),2)
            long_name = STRTRIM(STRING(vatt.LONG_NAME),2)
            units     = ' ['+STRTRIM(STRING(vatt.UNITS),2)+']'
            fillvalue = vatt._FILLVALUE

            base = FSC_Base_Filename(file[f])
            xtitle = long_name + units
            figure_title = source + ' (source) for ' + time

            IF KEYWORD_SET(verbose) THEN BEGIN
                PRINT, '** Loaded variable: ', xtitle
                HELP, gatt
                HELP, vatt
            ENDIF



            ; -----------------------------------------------------------------
            IF KEYWORD_SET(hist1d) THEN BEGIN
            ; -----------------------------------------------------------------

                opt = 'hist1d_'
                outfile = !SAVE_DIR + base + '_' + opt + vars[i]

                IF KEYWORD_SET(ratio) THEN $
                    outfile = outfile + '_ratio'
                IF KEYWORD_SET(ref) THEN $
                    outfile = outfile + '_compare_with_'+ref

                IF (vars[i] EQ 'ctt' OR vars[i] EQ 'ctp') THEN $
                    xtitle = ' for ' + time ELSE $
                    xtitle = units + ' for ' + time

                IF (vars[i] EQ 'cer') THEN ymax = 60.
                IF (vars[i] EQ 'cot') THEN ymax = 30.
                IF (vars[i] EQ 'cwp') THEN ymax = 30.
                IF (vars[i] EQ 'ctt') THEN ymax = 50.

                READ_SIM_NCDF, h1d, FILE=file[f], VAR_NAME=opt+vars[i]
                READ_SIM_NCDF, bin, FILE=file[f], VAR_NAME=opt+vars[i]+'_bin_border'
                
                save_as = outfile + '.eps'
                start_save, save_as, size='A4', /LANDSCAPE
                cs = 2.0

                IF (vars[i] EQ 'cer') THEN varn = 'ref' ELSE varn = vars[i]

                IF KEYWORD_SET(ref) THEN BEGIN
                    compare = {ref:ref, sat:sat, var:varn, $
                               year:STRMID(time, 0, 4), $
                               month:STRMID(time, 4, 2), dat:''}
                ENDIF

                CREATE_1DHIST, RESULT=h1d, VARNAME=varn, $
                    VARSTRING=opt+varn, CHARSIZE=cs, XTITLE=xtitle, $
                    YMAX=ymax, LEGEND_POSITION=legend_position, RATIO=ratio, $
                    BIN_BORDERS=bin, COMPARE=compare

                ymax = 0.

                end_save, save_as

            ENDIF


            ; -----------------------------------------------------------------
            IF KEYWORD_SET(map) THEN BEGIN
            ; -----------------------------------------------------------------

                opt = 'map_'
                outfile = !SAVE_DIR + base + '_' + opt + vars[i]

                save_as = outfile + '.eps'
                start_save, save_as, size='A4', /LANDSCAPE
                limit = [-90., -180., 90., 180.]
                void = WHERE(DATA EQ fillvalue, COMPLEMENT=good)

                READ_SIM_NCDF, lon, FILE=file[f], VAR_NAME='lon'
                READ_SIM_NCDF, lat, FILE=file[f], VAR_NAME='lat'
                GET_ERA_GRID, data, lon, lat, grid

                IF ~KEYWORD_SET(mini) THEN mini = MIN(data[good])
                IF ~KEYWORD_SET(maxi) THEN maxi = MAX(data[good])
                
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

            ENDIF


            ; -----------------------------------------------------------------
            IF KEYWORD_SET(ref) AND ~KEYWORD_SET(hist1d) THEN BEGIN
            ; -----------------------------------------------------------------

                PRINT, "** Compare simulated "+vars[i]+" with "+ref

                year = STRMID(time, 0, 4)
                month = STRMID(time, 4, 2) 

                compare_cci_with_clara, year, month, '', algo1='era-i',$
                    data=vars[i], ccifile=file[f], reference=ref, $
                    sat=sat, mini=mini, maxi=maxi , limit=limit, $
                    save_dir=!SAVE_DIR, land=land, sea=sea, cov=cov, $
                    other='rainbow', ctable='', level='l3c'

            ENDIF


        ENDFOR ; loop over variables

    ENDFOR ;loop over files

    SPAWN, './subtools/eps2png.sh '+!SAVE_DIR

    PRINT, "** TOTAL Elapsed Time: ", (SYSTIME(1)-STT)/60., " minutes"

;******************************************************************************
END ;end of program
;******************************************************************************