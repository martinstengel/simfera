@/../vali_gui/plot_l3.pro
@local_include_file.pro
;+
; NAME:
;   CLOUD_SIMULATOR
;
; PURPOSE:
;   Calculates monthly means of cloud-cci like parameters 
;   based on ERA-Interim reanalysis
;
; AUTHOR:
;   Dr. Martin Stengel
;   Deutscher Wetterdienst (DWD)
;   KU22, Climate-based satellite monitoring
;   martin.stengel@dwd.de
;
; CATEGORY:
;
;
; CALLING SEQUENCE:
;   cloud_simulator
;
; MODIFICATION HISTORY:
;   Written by Dr. Martin Stengel, 2014; 
;     grid_mean arrays as output; for comparison with model results
;   C. Schlundt, Jul 2015: program modifications - subroutines added
;   C. Schlundt, Jul 2015: incloud_mean arrays added
;                          (LWP and IWP weighted with CFC)
;   C. Schlundt, Sep 2015: binary CFC, CPH added and applied to LWP/IWP
;   C. Schlundt, Oct 2015: implementation of structures
;   C. Schlundt, Oct 2015: implementation of CWP
;   C. Schlundt, Oct 2015: implementation of COT
;   C. Schlundt, Oct 2015: implementation of SZA2d
;   C. Schlundt, Oct 2015: implementation of COT/CWP dayside only
;   C. Schlundt, Oct 2015: implementation of 1D Histograms
;   C. Schlundt, Nov 2015: implementation of 2D Histogram COT-CTP
;   C. Schlundt, Jan 2016: implementation of ireff, lreff as func(T,IWC/LWC)
;   C. Schlundt, Jan 2016: implementation of hist1d_ref
;   C. Schlundt, Jan 2016: clean up code
;   M. Stengel,  Jan 2016: scops-like method for COT, CWP, CFC (random overlap)
;   C. Schlundt, Jan 2016: speedup of search4cloud.pro
;   C. Schlundt, Jan 2016: scops type in config file: 1=random, 2=max/random
;   C. Schlundt, Jan 2016: Cloud_cci variable names implemented
;   C. Schlundt, Jan 2016: SZA < 75 equals daytime (previously 80 deg)
;   M. Stengel,  Jan 2016: pseudo_retrieval, i.e. SCOPS and more
;   M. Stengel,  Feb 2016: cph_day, cwp_allsky, lwp_allsky, iwp_allsky added
;   C. Schlundt, Mar 2016: code optimization
;
;******************************************************************************
PRO CLOUD_SIMULATOR, VERBOSE=verbose, LOGFILE=logfile, TEST=test, $
                     SYEAR=syear, EYEAR=eyear, SMONTH=smonth, EMONTH=emonth, $
                     RATIO=ratio, CONSTANT_CER=constant_cer, HELP=help
;******************************************************************************
    STT = SYSTIME(1)

    IF KEYWORD_SET(help) THEN BEGIN
        PRINT, ""
        PRINT, " *** THIS PROGRAM READS ERA-INTERIM REANALYSIS FILES AND",$
               " SIMULATES CLOUD_CCI CLOUD PARAMETERS ***"
        PRINT, ""
        PRINT, " Please, first copy the ""config_simulator.pro.template"" to",$
               " ""config_simulator.pro"" and modify the settings for your needs."
        PRINT, ""
        PRINT, " USAGE: "
        PRINT, " CLOUD_SIMULATOR, /test, /log, /ver "
        PRINT, " CLOUD_SIMULATOR, sy=2008, sm=7"
        PRINT, " CLOUD_SIMULATOR, sy=1979, ey=2014, sm=1, em=12"
        PRINT, ""
        PRINT, " Optional Keywords:"
        PRINT, " SYEAR          start year, which should be processed."
        PRINT, " EYEAR          end year."
        PRINT, " SMONTH         start month."
        PRINT, " EMONTH         end month."
        PRINT, " CONSTANT_CER   using constant eff. radii for COT calculation."
        PRINT, " VERBOSE        increase output verbosity."
        PRINT, " LOGFILE        creates journal logfile."
        PRINT, " TEST           output based on the first day only."
        PRINT, " RATIO          adds liquid cloud fraction to HIST1D plot."
        PRINT, " HELP           prints this message."
        PRINT, ""
        RETURN
    ENDIF


    ; for intermediate & final plotting
    vars  = ['cer','cot','cwp','ctt','ctp']
    vars2 = ['cfc','cph','cth','lwp','iwp',$
             'cot_liq','cot_ice','cer_liq','cer_ice']


    IF KEYWORD_SET(verbose) THEN PRINT, '** Import user setttings'

    CONFIG_SIMULATOR, PATHS=path, TIMES=time, SETTINGS=set, $ 
        SYEAR=syear, EYEAR=eyear, SMONTH=smonth, EMONTH=emonth, $ 
        HIST_INFO=his, CER_INFO=cer_info, TEST=test


    !EXCEPT=2 ; detects errors/warnings
    DEFSYSV, '!SAVE_DIR', path.FIG

    IF KEYWORD_SET(logfile) THEN BEGIN 
        aTimeStamp = TIMESTAMP(format)
        JOURNAL, path.out + 'journal_' + set.COT_STR + aTimeStamp + '.pro'
    ENDIF

    PRINT, FORMAT='(A, A-100)', '** INP:     ', path.INP
    PRINT, FORMAT='(A, A-100)', '** OUT:     ', path.OUT
    PRINT, FORMAT='(A, A-100)', '** FIG:     ', path.FIG
    PRINT, FORMAT='(A, E12.1)',  '** COT-thv: ', set.COT
    PRINT, FORMAT='(A, A-100)', '** SCOPS:   ', set.SCOPS
    PRINT, FORMAT='(A, A-100)', '** MPC:     ', set.MPC

    IF KEYWORD_SET(constant_cer) THEN BEGIN 
        mess = "** CWP & COT based on FIXED CER [um]"
        fmt = '(A, " ! ", "cer_water =", F5.1, "; cer_ice =", F5.1)'
        PRINT, FORMAT=fmt, mess, [cer_info.water, cer_info.ice]
    ENDIF ELSE BEGIN
        mess = "** CWP & COT based on ERA-I: CER(T,CWC) [um]"
        PRINT, FORMAT='(A, " ! ")', mess
    ENDELSE


    ; loop over years and months
    FOR ii1=0, time.NY-1 DO BEGIN
        FOR jj1=0, time.NM-1 DO BEGIN

            SMM = SYSTIME(1)
            year  = time.YEARS[ii1]
            month = time.MONTHS[jj1]
            counti = 0

            ff = FINDFILE(path.inp+year+month+'/'+'*'+year+month+'*plev')
            numff = N_ELEMENTS(ff)
            strff = STRTRIM(numff,2)
            strym = STRING(year) + '/' + STRING(month)

            IF(N_ELEMENTS(ff) GT 1) THEN BEGIN

                PRINT, '** ', strff, ' ERA-Interim InputFiles for ', strym

                FOR fidx=0,N_ELEMENTS(ff)-1,1 DO BEGIN ;loop over files

                    file0 = ff[fidx]
                    file1 = file0+'.nc'

                    IF(is_file(file0) AND (NOT is_file(file1))) THEN BEGIN
                        PRINT,'** Converting: ' + file0
                        SPAWN,'cdo -f nc copy ' + file0 + ' ' + file1
                    ENDIF

                    IF(is_file(file1)) THEN BEGIN

                        READ_ERA_NCFILE, file1, input

                        PRINT, '** ',STRTRIM(counti+1,2),'. ',input.FILE

                        IF(counti EQ 0) THEN BEGIN

                            INIT_ERA_GRID, input, grid 
                            READ_ERA_SSTFILE, path.SST, grid, sst, void, MAP=test
                            lsm2d = INIT_LSM_ARRAY(grid, sst, void, MAP=test)
                            INIT_OUT_ARRAYS, grid, his, means, counts

                        ENDIF
                        counti++

                        ; initialize solar zenith angle 2D array
                        sza2d = INIT_SZA_ARRAY(input, grid, MAP=test)

                        ; lwc and iwc weighted by cc
                        incloud = CALC_INCLOUD_CWC( input, grid )

                        ; calculate: CWP/COT/CER per layer based in incloud CWC
                        CALC_CLD_VARS, incloud.LWC, incloud.IWC, input, grid, $
                            lsm2d, cer_info, cwp_lay, cot_lay, cer_lay, $ 
                            CONSTANT_CER=constant_cer, VERBOSE=verbose

                        PR = SYSTIME(1)
                        ; means (in/out), temps (out)
                        CORE, input, grid, sza2d, set.SCOPS, $ 
                            cwp_lay, cot_lay, cer_lay, set.COT, set.MPC, $
                            his, means, temps, TEST=test
                        PRINT, "** Core: ", (SYSTIME(1)-PR)/60., " minutes"

                        ; sum up cloud parameters
                        SUMUP_VARS, means, counts, temps

                        ; count number of files
                        counts.raw++

                        ; delete tmp arrays
                        UNDEFINE, sza2d, temps
                        UNDEFINE, cwp_lay, cot_lay, cer_lay

                    ENDIF ;end of IF(is_file(file1))

                ENDFOR ;end of file loop

                ; calculate averages
                MEAN_VARS, means, counts

                WMM = SYSTIME(1)
                ; write output files
                WRITE_MONTHLY_MEAN, path.out, year, month, grid, input, his, $ 
                    set.COT_STR, set.COT, means, counts, set.SCOPS, set.MPC, $
                    TEST=test
                PRINT, "** WriteFile: ", (SYSTIME(1)-WMM)/60., " minutes"

                ; delete final arrays before next cycle starts
                UNDEFINE, means, counts

            ENDIF ELSE BEGIN 
                PRINT, '! NO ERA-Interim InputFiles found for ', strym, ' !'
            ENDELSE

           PRINT, "** Elapsed Time: ", (SYSTIME(1)-SMM)/60., " minutes"

        ENDFOR ;end of month loop
    ENDFOR ;end of year loop

    ; End journaling:
    IF KEYWORD_SET(logfile) THEN JOURNAL

    PRINT, "** TOTAL Elapsed Time: ", (SYSTIME(1)-STT)/60., " minutes"

;******************************************************************************
END ;end of program
;******************************************************************************
