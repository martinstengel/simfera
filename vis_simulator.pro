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
PRO VIS_SIMULATOR, VERBOSE=verbose, ALL=all, ZONAL=zonal, COMPARE=compare, $
                   HIST1D=hist1d, MAP=map, REFS=refs, SAT=sat, $
                   VARS=vars, RATIO=ratio, NOPNG=nopng, HELP=help
;******************************************************************************
    STT = SYSTIME(1)

    ; MODIFY settings
    CONFIG_VIS, SETTINGS=set, HIST1D=hist1d, VARS=vars, REFS=refS
    HELP, set
    DEFSYSV, '!SAVE_DIR', set.OUT_PWD

    IF KEYWORD_SET(help) THEN BEGIN
        PRINT, ""
        PRINT, " *** THIS PROGRAM VISUALIZES THE SIMULATOR OUTPUT AND",$
               " COMPARES IT WITH REFERENCE DATA ***"
        PRINT, ""
        PRINT, " USAGE: "
        PRINT, " VIS_SIMULATOR, /map"
        PRINT, " VIS_SIMULATOR, /all, refs='cci', sat='NOAA15' "
        PRINT, " VIS_SIMULATOR, /zonal, vars='cfc' "
        PRINT, " VIS_SIMULATOR, vars='ctp', /hist1d"
        PRINT, " VIS_SIMULATOR, vars='cwp,cot', /hist1d, refs='cci', sat='NOAA18' "
        PRINT, " VIS_SIMULATOR, /compare, vars='cfc', refs='cci', sat='NOAA18' "
        PRINT, ""
        PRINT, " Optional Keywords:"
        PRINT, " VARS           list of parameters to be plotted, default is: ", vars
        PRINT, " MAP            creates 2D maps"
        PRINT, " HIST1D         creates 1D histogram plots"
        PRINT, " COMPARE        compares simulated and observed data (4 plots)"
        PRINT, " ZONAL          creates zonal mean plot, optional incl.: ",$
                                "options are: ", set.REFDATA
        PRINT, " ALL            creates all figures for given variables"
        PRINT, " REFS           options are: ", set.REFDATA
        PRINT, " SAT            REF requires SAT in some cases, e.g. sat='NOAA18'"
        PRINT, " VERBOSE        increase output verbosity."
        PRINT, " RATIO          adds liquid cloud fraction to HIST1D plot."
        PRINT, " HELP           prints this message."
        PRINT, ""
        RETURN
    ENDIF


    FOR f=0, set.NFILES-1 DO BEGIN ; loop over files

        file = set.FILES[f]
        PRINT, "** Working on: ", file

        FOR i=0, N_ELEMENTS(vars)-1 DO BEGIN ; loop over variables

            ; read simulator output file
            READ_SIM_NCDF, data, FILE=file, VAR_NAME=vars[i], $ 
                GLOB_ATTR=gatt, VAR_ATTR=vatt

            source    = STRTRIM(STRING(gatt.SOURCE),2)
            time      = STRTRIM(STRING(gatt.TIME_COVERAGE_START),2)
            cot_thv   = STRTRIM(STRING(gatt.COT_THV,FORMAT='(F4.2)'),2)
            nfiles    = STRTRIM(STRING(gatt.NUMBER_OF_FILES),2)
            scops     = STRTRIM(STRING(gatt.SCOPS_TYPE),2)
            long_name = STRTRIM(STRING(vatt.LONG_NAME),2)
            units     = ' ['+STRTRIM(STRING(vatt.UNITS),2)+']'
            fillvalue = vatt._FILLVALUE

            base = FSC_Base_Filename(file)
            xtitle = long_name + units
            figure_title = source + ' (source) for ' + time

            IF KEYWORD_SET(verbose) THEN BEGIN
                PRINT, '** Loaded variable: ', xtitle
                HELP, gatt
                HELP, vatt
            ENDIF


            IF KEYWORD_SET(hist1d) OR KEYWORD_SET(all) THEN $
                PLOT_SIM_HIST, file, vars[i], !SAVE_DIR, base, xtitle, $
                               units, time, SAT=sat, REFS=refs, RATIO=ratio


            IF KEYWORD_SET(map) OR KEYWORD_SET(all) THEN $ 
                PLOT_SIM_MAPS, file, vars[i], !SAVE_DIR, data, fillvalue, $
                               set.MINI_MAXI[0,i], set.MINI_MAXI[1,i], $
                               base, figure_title, xtitle


            IF KEYWORD_SET(compare) OR KEYWORD_SET(all) THEN $
                PLOT_SIM_COMPARE_WITH, file, refs, vars[i], !SAVE_DIR, $
                                       time, SAT=sat


            IF KEYWORD_SET(zonal) OR KEYWORD_SET(all) THEN $
                PLOT_SIM_COMPARE_ZONAL, file, vars[i], time, $
                                        REFS=refs, SAT=sat


        ENDFOR ; loop over variables
    ENDFOR ;loop over files

    IF ~KEYWORD_SET(nopng) THEN SPAWN, set.EPS2PNG + !SAVE_DIR

    PRINT, "** TOTAL Elapsed Time: ", (SYSTIME(1)-STT)/60., " minutes"

;******************************************************************************
END ;end of program
;******************************************************************************
