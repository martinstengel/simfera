@/../../vali_gui/plot_l3.pro
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
;   see help below
;
; MODIFICATION HISTORY:
;   Jan 2016, first version
;
;******************************************************************************
PRO VIS_SIMULATOR, VERBOSE=verbose, MONTHLY=monthly, $
                   TIMESERIES=timeseries, SNAPSHOT=snapshot, $
                   VARS=vars, RATIO=ratio, REFS=refs, SAT=sat, HELP=help
;******************************************************************************
    STT = SYSTIME(1)

    IF KEYWORD_SET(help) THEN BEGIN
        PRINT, ""
        PRINT, " *** THIS PROGRAM VISUALIZES THE SIMULATOR OUTPUT AND",$
               " COMPARES IT WITH REFERENCE DATA ***"
        PRINT, ""
        PRINT, " USAGE: "
        PRINT, " VIS_SIMULATOR, /snapshot "
        PRINT, " VIS_SIMULATOR, /timeseries [,vars='cfc'] "
        PRINT, " VIS_SIMULATOR, /monthly, refs='cci', sat='NOAA18' [,vars='cfc'] "
        PRINT, " VIS_SIMULATOR, /snap, /time, /mon, refs='cci', sat='NOAA18' "
        PRINT, ""
        PRINT, " Optional Keywords:"
        PRINT, " VARS           list of parameters to be plotted, e.g. vars='cfc,ctp,cot_liq' "
        PRINT, " REFS           reference dataset, e.g. refs='cci,gac2,mod2,myd2,pmx' "
        PRINT, " SAT            REFS requires SAT, e.g. sat='NOAA18'"
        PRINT, " RATIO          adds liquid cloud fraction to HIST1D plot."
        PRINT, " MONTHLY        compares simulated and observed monthly data (4 plots)"
        PRINT, " TIMESERIES     compares simulated and observed timeseries data (4 plots) ", $
               "using SAV-files (1982-2014); only for CCI-NOAAPM available"
        PRINT, " SNAPSHOT       plots model profiles, matrices, subcolumn arrays ", $
               "for a specific timeslot of ERA-I reanalysis, e.g. /snapshot "
        PRINT, " VERBOSE        increase output verbosity."
        PRINT, " HELP           prints this message."
        PRINT, ""
        RETURN
    ENDIF

    IF ~KEYWORD_SET(refs) THEN refs = 'cci'
    IF ~KEYWORD_SET(sat) THEN sat = 'NOAA18'

    ; MODIFY settings
    CONFIG_VIS, cfg, VARS=vars, REFS=refs
    HELP, cfg
    DEFSYSV, '!SAVE_DIR', cfg.FIGOUT

    aTimeStamp = TIMESTAMP(format)
    JOURNAL, !SAVE_DIR + '/journal_vis_simulator' + aTimeStamp + '.pro'


    IF KEYWORD_SET(snapshot) THEN BEGIN
        FOR f=0, cfg.SFILES_COUNT-1 DO BEGIN
            osav = !SAVE_DIR+'snapshots/'
            validres = VALID_DIR( osav )
            IF(validres EQ 0) THEN creatres = CREATE_DIR( osav )
            file = cfg.SFILES[f]

            PRINT, "** Working on: ", file 
            ;PLOT_ARRAYS, file, osav
            ;PLOT_PROFILES, file, 'cfc_profile', osav, /all
            PLOT_MATRICES, file, 'cfc_matrix', osav, /all
            ;PLOT_PROFILES, file, 'cfc_profile', osav
            ;PLOT_MATRICES, file, 'cfc_matrix', osav
            ;PLOT_PROFILES, file, 'cot_profile', osav
            ;PLOT_MATRICES, file, 'cot_matrix', osav
            ;PLOT_PROFILES, file, 'cwp_profile', osav
            ;PLOT_MATRICES, file, 'cwp_matrix', osav
            ;PLOT_PROFILES, file, 'cer_profile', osav
            ;PLOT_MATRICES, file, 'cer_matrix', osav
            ;PLOT_MATRICES, file, 'cph_matrix', osav 
        ENDFOR
    ENDIF


    ; based on pre-processed SAV-files!
    IF KEYWORD_SET(timeseries) THEN BEGIN 
        FOR i=0, N_ELEMENTS(vars)-1 DO BEGIN ; loop over variables 
            PLOT_PCMULTI, vars[i], refs, 'NOAAPM', outpwd=cfg.FIGOUT, $
                mini=cfg.MINI_MAXI[0,i], maxi=cfg.MINI_MAXI[1,i]
        ENDFOR
    ENDIF


    IF KEYWORD_SET(monthly) THEN BEGIN
        FOR f=0, cfg.MFILES_COUNT-1 DO BEGIN

            file = cfg.MFILES[f]
            PRINT, "** Working on: ", file

            FOR i=0, N_ELEMENTS(vars)-1 DO BEGIN ; loop over variables

                ; read simulator output file
                READ_SIM_NCDF, data, FILE=file, VAR_NAME=vars[i], $ 
                    GLOB_ATTR=gatt, VAR_ATTR=vatt

                source    = STRTRIM(STRING(gatt.SOURCE),2)
                time      = STRTRIM(STRING(gatt.TIME_COVERAGE_START),2)
                cot_thv   = STRTRIM(STRING(gatt.COT_THV,FORMAT='(F4.2)'),2)
                nfiles    = STRTRIM(STRING(gatt.NUMBER_OF_FILES),2)
                scops     = STRTRIM(STRING(gatt.SCOPS),2)
                long_name = STRTRIM(STRING(vatt.LONG_NAME),2)
                units     = ' ['+STRTRIM(STRING(vatt.UNITS),2)+']'
                fillvalue = vatt._FILLVALUE

                base = FSC_Base_Filename(file)
                xtitle = long_name + units
                figure_title = source + ' (source) for ' + time

                mini = cfg.MINI_MAXI[0,i]
                maxi = cfg.MINI_MAXI[1,i]

                IF KEYWORD_SET(verbose) THEN BEGIN
                    PRINT, '** Loaded variable: ', xtitle
                    HELP, gatt
                    HELP, vatt
                ENDIF

                PLOT_SIM_COMPARE_HIST, file, vars[i], !SAVE_DIR+'monthly/', $
                    base, xtitle, units, time, SAT=sat, REFS=refs, RATIO=ratio

                PLOT_SIM_COMPARE_JCH, file, vars[i], time, !SAVE_DIR+'monthly/', $
                    base, mini, maxi, fillvalue, REFS=refs, SAT=sat

                PLOT_SIM_COMPARE_WITH, file, refs, vars[i], $
                    !SAVE_DIR+'monthly/', time, mini, maxi, SAT=sat

                ref_data = 'cci,gac2,mod2,myd2,pmx'
                ref_list = STRSPLIT(ref_data, /EXTRACT,',')
                PLOT_SIM_COMPARE_ZONAL, file, vars[i], time, !SAVE_DIR+'monthly/', $ 
                    base, mini, maxi, ref_list, sat
                    ;base, mini, maxi, refs, sat

                ;PLOT_SIM_MAPS, file, vars[i], !SAVE_DIR+'monthly/', data, $
                ;       fillvalue, mini, maxi, base, figure_title, xtitle

            ENDFOR ; loop over variables
        ENDFOR ;loop over files
    ENDIF ; monthly

    PRINT, "** TOTAL Elapsed Time: ", (SYSTIME(1)-STT)/60., " minutes"

    ; End journaling:
    JOURNAL

;******************************************************************************
END ;end of program
;******************************************************************************
