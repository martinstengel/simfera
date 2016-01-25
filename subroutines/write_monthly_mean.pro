;-----------------------------------------------------------------------------
;-- write netcdf histograms & monthly mean (average) output
;-----------------------------------------------------------------------------
PRO WRITE_MONTHLY_MEAN, path_out, year, month, grd, inp, hist, $
                        thv_str, thv_val, means, counts, scops_type
;-----------------------------------------------------------------------------

    dim_time   = 1
    fyear      = FLOAT(year)
    fmonth     = FLOAT(month)
    dayi_start = 1
    dayi_end   = daysinmonth(fyear,fmonth)
    tbo        = DBLARR(2,1)
    tref       = JULDAY(1,1,1970,0,0,0)
    tttt       = JULDAY(fmonth,dayi_start,fyear,0,0,0)
    tttt2      = JULDAY(fmonth,dayi_end,fyear,23,59,59)
    tbo[0,0]   = tttt-tref
    tbo[1,0]   = tttt2-tref
    itime      = tttt-tref

    IF (scops_type EQ 1) THEN st = 'random' ELSE st='max/random'
    ststr = STRTRIM(STRING(scops_type),2)

    file_out = 'SimpSimu_MM'+year+month+'_'+thv_str+'_scops'+ststr+'.nc'
    clobber  = 1

    lon = inp.lon ;degrees_east
    lat = inp.lat ;degrees_north

    ; -- Create netCDF output file
    id = NCDF_CREATE(path_out + file_out, CLOBBER = clobber)

    NCDF_ATTPUT, id, /GLOBAL, "Source" , "ERA-Interim" ;
    NCDF_ATTPUT, id, /GLOBAL, "TIME_COVERAGE_START" , "" + year + month
    NCDF_ATTPUT, id, /GLOBAL, "TIME_COVERAGE_RESOLUTION", "P1M"
    NCDF_ATTPUT, id, /GLOBAL, "cot_thv", thv_val
    NCDF_ATTPUT, id, /GLOBAL, "number_of_files", counts.RAW
    NCDF_ATTPUT, id, /GLOBAL, "scops_type", scops_type 

    ; -- dimensions
    dim_x_id  = NCDF_DIMDEF(id, 'lon', grd.xdim)
    dim_y_id  = NCDF_DIMDEF(id, 'lat', grd.ydim)
    time_id   = NCDF_DIMDEF(id, 'time', dim_time)
    dim_phase = NCDF_DIMDEF(id, 'hist_phase', hist.phase_dim)
    ; hist2d
    hist2d_cot_bin_border = NCDF_DIMDEF(id, 'hist2d_cot_bin_border', hist.cot1d_dim)
    hist2d_cot_bin_centre = NCDF_DIMDEF(id, 'hist2d_cot_bin_centre', hist.cot_bin1d_dim)
    hist2d_ctp_bin_border = NCDF_DIMDEF(id, 'hist2d_ctp_bin_border', hist.ctp1d_dim)
    hist2d_ctp_bin_centre = NCDF_DIMDEF(id, 'hist2d_ctp_bin_centre', hist.ctp_bin1d_dim)
    ; hist1d
    hist1d_cot_bin_border = NCDF_DIMDEF(id, 'hist1d_cot_bin_border', hist.cot1d_dim)
    hist1d_cot_bin_centre = NCDF_DIMDEF(id, 'hist1d_cot_bin_centre', hist.cot_bin1d_dim)
    hist1d_ctp_bin_border = NCDF_DIMDEF(id, 'hist1d_ctp_bin_border', hist.ctp1d_dim)
    hist1d_ctp_bin_centre = NCDF_DIMDEF(id, 'hist1d_ctp_bin_centre', hist.ctp_bin1d_dim)
    hist1d_ctt_bin_border = NCDF_DIMDEF(id, 'hist1d_ctt_bin_border', hist.ctt1d_dim)
    hist1d_ctt_bin_centre = NCDF_DIMDEF(id, 'hist1d_ctt_bin_centre', hist.ctt_bin1d_dim)
    hist1d_cwp_bin_border = NCDF_DIMDEF(id, 'hist1d_cwp_bin_border', hist.cwp1d_dim)
    hist1d_cwp_bin_centre = NCDF_DIMDEF(id, 'hist1d_cwp_bin_centre', hist.cwp_bin1d_dim)
    hist1d_cer_bin_border = NCDF_DIMDEF(id, 'hist1d_cer_bin_border', hist.cer1d_dim)
    hist1d_cer_bin_centre = NCDF_DIMDEF(id, 'hist1d_cer_bin_centre', hist.cer_bin1d_dim)

    ; -- time: monthly mean output
    vid  = NCDF_VARDEF(id, 'time', [time_id], /DOUBLE)

    ; -- longitude
    vid  = NCDF_VARDEF(id, 'lon', [dim_x_id], /FLOAT)
    NCDF_ATTPUT, id, 'lon', 'long_name', 'longitude'
    NCDF_ATTPUT, id, 'lon', 'units', 'degrees_east'

    ; -- latitude
    vid  = NCDF_VARDEF(id, 'lat', [dim_y_id], /FLOAT)
    NCDF_ATTPUT, id, 'lat', 'long_name', 'latitude'
    NCDF_ATTPUT, id, 'lat', 'units', 'degrees_north'


    ; === HISTOGRAMS ===

    vid  = NCDF_VARDEF(id, 'hist_phase', [dim_phase], /DOUBLE)
    NCDF_ATTPUT, id, 'hist_phase', 'long_name', 'phase histogram bins'
    NCDF_ATTPUT, id, 'hist_phase', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist2d_cot_bin_border', [hist2d_cot_bin_border], /DOUBLE)
    NCDF_ATTPUT, id, 'hist2d_cot_bin_border', 'long_name', 'cot histogram border values'
    NCDF_ATTPUT, id, 'hist2d_cot_bin_border', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist2d_cot_bin_centre', [hist2d_cot_bin_centre], /DOUBLE)
    NCDF_ATTPUT, id, 'hist2d_cot_bin_centre', 'long_name', 'cot histogram bins'
    NCDF_ATTPUT, id, 'hist2d_cot_bin_centre', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist2d_ctp_bin_border', [hist2d_ctp_bin_border], /DOUBLE)
    NCDF_ATTPUT, id, 'hist2d_ctp_bin_border', 'long_name', 'ctp histogram border values'
    NCDF_ATTPUT, id, 'hist2d_ctp_bin_border', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist2d_ctp_bin_centre', [hist2d_ctp_bin_centre], /DOUBLE)
    NCDF_ATTPUT, id, 'hist2d_ctp_bin_centre', 'long_name', 'ctp histogram bins'
    NCDF_ATTPUT, id, 'hist2d_ctp_bin_centre', 'units', ' '


    ; -- cloud effective radius 1d histogram ----------------------------------
    vid  = NCDF_VARDEF(id, 'hist1d_cer_bin_border', [hist1d_cer_bin_border], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_cer_bin_border', 'long_name', 'histogram_cer1d'
    NCDF_ATTPUT, id, 'hist1d_cer_bin_border', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_cer_bin_centre', [hist1d_cer_bin_centre], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_cer_bin_centre', 'long_name', 'histogram_ref_bin1d'
    NCDF_ATTPUT, id, 'hist1d_cer_bin_centre', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_cer', $
        [dim_x_id, dim_y_id, hist1d_cer_bin_centre, dim_phase, time_id], /LONG)
    NCDF_ATTPUT, id, 'hist1d_cer', 'long_name', 'hist1d_cer'
    NCDF_ATTPUT, id, 'hist1d_cer', 'units', 'counts'
    NCDF_ATTPUT, id, 'hist1d_cer', '_FillValue', -999l, /LONG


    ; -- cloud top pressure 1d histogram --------------------------------------
    vid  = NCDF_VARDEF(id, 'hist1d_ctp_bin_border', [hist1d_ctp_bin_border], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_ctp_bin_border', 'long_name', 'histogram_ctp1d'
    NCDF_ATTPUT, id, 'hist1d_ctp_bin_border', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_ctp_bin_centre', [hist1d_ctp_bin_centre], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_ctp_bin_centre', 'long_name', 'histogram_ctp_bin1d'
    NCDF_ATTPUT, id, 'hist1d_ctp_bin_centre', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_ctp', $
        [dim_x_id, dim_y_id, hist1d_ctp_bin_centre, dim_phase, time_id], /LONG)
    NCDF_ATTPUT, id, 'hist1d_ctp', 'long_name', 'hist1d_ctp'
    NCDF_ATTPUT, id, 'hist1d_ctp', 'units', 'counts'
    NCDF_ATTPUT, id, 'hist1d_ctp', '_FillValue', -999l, /LONG


    ; -- cloud top temperature 1d histogram ------------------------------------
    vid  = NCDF_VARDEF(id, 'hist1d_ctt_bin_border', [hist1d_ctt_bin_border], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_ctt_bin_border', 'long_name', 'histogram_ctt1d'
    NCDF_ATTPUT, id, 'hist1d_ctt_bin_border', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_ctt_bin_centre', [hist1d_ctt_bin_centre], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_ctt_bin_centre', 'long_name', 'histogram_ctt_bin1d'
    NCDF_ATTPUT, id, 'hist1d_ctt_bin_centre', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_ctt', $
        [dim_x_id, dim_y_id, hist1d_ctt_bin_centre, dim_phase, time_id], /LONG)
    NCDF_ATTPUT, id, 'hist1d_ctt', 'long_name', 'hist1d_ctt'
    NCDF_ATTPUT, id, 'hist1d_ctt', 'units', 'counts'
    NCDF_ATTPUT, id, 'hist1d_ctt', '_FillValue', -999l, /LONG


    ; -- cloud water path 1d histogram ----------------------------------------
    vid  = NCDF_VARDEF(id, 'hist1d_cwp_bin_border', [hist1d_cwp_bin_border], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_cwp_bin_border', 'long_name', 'histogram_cwp1d'
    NCDF_ATTPUT, id, 'hist1d_cwp_bin_border', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_cwp_bin_centre', [hist1d_cwp_bin_centre], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_cwp_bin_centre', 'long_name', 'histogram_cwp_bin1d'
    NCDF_ATTPUT, id, 'hist1d_cwp_bin_centre', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_cwp', $
        [dim_x_id, dim_y_id, hist1d_cwp_bin_centre, dim_phase, time_id], /LONG)
    NCDF_ATTPUT, id, 'hist1d_cwp', 'long_name', 'hist1d_cwp'
    NCDF_ATTPUT, id, 'hist1d_cwp', 'units', 'counts'
    NCDF_ATTPUT, id, 'hist1d_cwp', '_FillValue', -999l, /LONG


    ; -- cloud optical thickness 1d histogram ----------------------------------
    vid  = NCDF_VARDEF(id, 'hist1d_cot_bin_border', [hist1d_cot_bin_border], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_cot_bin_border', 'long_name', 'histogram_cot1d'
    NCDF_ATTPUT, id, 'hist1d_cot_bin_border', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_cot_bin_centre', [hist1d_cot_bin_centre], /FLOAT)
    NCDF_ATTPUT, id, 'hist1d_cot_bin_centre', 'long_name', 'histogram_cot_bin1d'
    NCDF_ATTPUT, id, 'hist1d_cot_bin_centre', 'units', ' '

    vid  = NCDF_VARDEF(id, 'hist1d_cot', $
        [dim_x_id, dim_y_id, hist1d_cot_bin_centre, dim_phase, time_id], /LONG)
    NCDF_ATTPUT, id, 'hist1d_cot', 'long_name', 'hist1d_cot'
    NCDF_ATTPUT, id, 'hist1d_cot', 'units', 'counts'
    NCDF_ATTPUT, id, 'hist1d_cot', '_FillValue', -999l, /LONG
    
    ; -- hist2d_cot_ctp
    vid  = NCDF_VARDEF(id, 'hist2d_cot_ctp', $
        [dim_x_id, dim_y_id, hist2d_cot_bin_centre, hist2d_ctp_bin_centre, dim_phase, time_id], /LONG)
    NCDF_ATTPUT, id, 'hist2d_cot_ctp', 'long_name', 'hist2d_cot_ctp'
    NCDF_ATTPUT, id, 'hist2d_cot_ctp', 'units', 'counts'
    NCDF_ATTPUT, id, 'hist2d_cot_ctp', '_FillValue', -999l, /LONG



    ; === MONTHLY MEANS ===

    vid  = NCDF_VARDEF(id, 'ctp', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'ctp', '_FillValue', -999.
    NCDF_ATTPUT, id, 'ctp', 'long_name', 'cloud top pressure'
    NCDF_ATTPUT, id, 'ctp', 'units', 'hPa'

    vid  = NCDF_VARDEF(id, 'cth', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cth', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cth', 'long_name', 'cloud top height'
    NCDF_ATTPUT, id, 'cth', 'units', 'km'

    vid  = NCDF_VARDEF(id, 'ctt', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'ctt', '_FillValue', -999.
    NCDF_ATTPUT, id, 'ctt', 'long_name', 'cloud top temperature'
    NCDF_ATTPUT, id, 'ctt', 'units', 'K'

    vid  = NCDF_VARDEF(id, 'cph', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cph', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cph', 'long_name', 'cloud phase'
    NCDF_ATTPUT, id, 'cph', 'units', ' '

    vid  = NCDF_VARDEF(id, 'cfc', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cfc', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cfc', 'long_name', 'cloud fraction'
    NCDF_ATTPUT, id, 'cfc', 'units', ' '

    vid  = NCDF_VARDEF(id, 'nobs', [dim_x_id,dim_y_id,time_id], /LONG)
    NCDF_ATTPUT, id, 'nobs', 'long_name', 'number of observations'
    NCDF_ATTPUT, id, 'nobs', 'units', ' '

    vid  = NCDF_VARDEF(id, 'cot', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cot', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cot', 'long_name', 'cloud optical thickness'
    NCDF_ATTPUT, id, 'cot', 'units', ' '

    vid  = NCDF_VARDEF(id, 'cot_liq', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cot_liq', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cot_liq', 'long_name', 'liquid cloud optical thickness'
    NCDF_ATTPUT, id, 'cot_liq', 'units', ' '

    vid  = NCDF_VARDEF(id, 'cot_ice', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cot_ice', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cot_ice', 'long_name', 'ice cloud optical thickness'
    NCDF_ATTPUT, id, 'cot_ice', 'units', ' '

    vid  = NCDF_VARDEF(id, 'cwp', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cwp', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cwp', 'long_name', 'cloud water path'
    NCDF_ATTPUT, id, 'cwp', 'units', 'g/m^2'

    vid  = NCDF_VARDEF(id, 'lwp', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'lwp', '_FillValue', -999.
    NCDF_ATTPUT, id, 'lwp', 'long_name', 'cloud liquid water path'
    NCDF_ATTPUT, id, 'lwp', 'units', 'g/m^2'

    vid  = NCDF_VARDEF(id, 'cer', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cer', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cer', 'long_name', 'cloud effective radius'
    NCDF_ATTPUT, id, 'cer', 'units', 'micrometer'

    vid  = NCDF_VARDEF(id, 'cer_liq', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cer_liq', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cer_liq', 'long_name', 'water effective radius'
    NCDF_ATTPUT, id, 'cer_liq', 'units', 'micrometer'

    vid  = NCDF_VARDEF(id, 'cer_ice', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'cer_ice', '_FillValue', -999.
    NCDF_ATTPUT, id, 'cer_ice', 'long_name', 'ice effective radius'
    NCDF_ATTPUT, id, 'cer_ice', 'units', 'micrometer'

    vid  = NCDF_VARDEF(id, 'nobs_lwp', [dim_x_id,dim_y_id,time_id], /LONG)
    NCDF_ATTPUT, id, 'nobs_lwp', 'long_name', 'number of observations'
    NCDF_ATTPUT, id, 'nobs_lwp', 'units', ' '

    vid  = NCDF_VARDEF(id, 'iwp', [dim_x_id,dim_y_id,time_id], /FLOAT)
    NCDF_ATTPUT, id, 'iwp', '_FillValue', -999.
    NCDF_ATTPUT, id, 'iwp', 'long_name', 'cloud ice water path'
    NCDF_ATTPUT, id, 'iwp', 'units', 'g/m^2'

    vid  = NCDF_VARDEF(id, 'nobs_iwp', [dim_x_id,dim_y_id,time_id], /LONG)
    NCDF_ATTPUT, id, 'nobs_iwp', 'long_name', 'number of observations'
    NCDF_ATTPUT, id, 'nobs_iwp', 'units', ' '

    NCDF_CONTROL, id, /ENDEF ;Exit define mode


    ; -- general
    NCDF_VARPUT, id, 'time', itime
    NCDF_VARPUT, id, 'lon', lon
    NCDF_VARPUT, id, 'lat', lat

    ; -- histograms
    NCDF_VARPUT, id, 'hist_phase',hist.phase
    NCDF_VARPUT, id, 'hist2d_cot_ctp',means.hist2d_cot_ctp
    NCDF_VARPUT, id, 'hist2d_cot_bin_border',hist.cot1d
    NCDF_VARPUT, id, 'hist2d_cot_bin_centre',hist.cot_bin1d
    NCDF_VARPUT, id, 'hist2d_ctp_bin_border',hist.ctp1d
    NCDF_VARPUT, id, 'hist2d_ctp_bin_centre',hist.ctp_bin1d
    NCDF_VARPUT, id, 'hist1d_ctp',means.hist1d_ctp
    NCDF_VARPUT, id, 'hist1d_ctp_bin_border', hist.ctp1d
    NCDF_VARPUT, id, 'hist1d_ctp_bin_centre', hist.ctp_bin1d
    NCDF_VARPUT, id, 'hist1d_ctt',means.hist1d_ctt
    NCDF_VARPUT, id, 'hist1d_ctt_bin_border', hist.ctt1d
    NCDF_VARPUT, id, 'hist1d_ctt_bin_centre', hist.ctt_bin1d
    NCDF_VARPUT, id, 'hist1d_cwp',means.hist1d_cwp
    NCDF_VARPUT, id, 'hist1d_cwp_bin_border', hist.cwp1d
    NCDF_VARPUT, id, 'hist1d_cwp_bin_centre', hist.cwp_bin1d
    NCDF_VARPUT, id, 'hist1d_cot',means.hist1d_cot
    NCDF_VARPUT, id, 'hist1d_cot_bin_border', hist.cot1d
    NCDF_VARPUT, id, 'hist1d_cot_bin_centre', hist.cot_bin1d
    NCDF_VARPUT, id, 'hist1d_cer',means.hist1d_cer
    NCDF_VARPUT, id, 'hist1d_cer_bin_border', hist.cer1d
    NCDF_VARPUT, id, 'hist1d_cer_bin_centre', hist.cer_bin1d


    NCDF_VARPUT, id, 'ctp', means.ctp
    NCDF_VARPUT, id, 'cth', means.cth
    NCDF_VARPUT, id, 'ctt', means.ctt
    NCDF_VARPUT, id, 'cph', means.cph
    NCDF_VARPUT, id, 'cfc', means.cfc
    NCDF_VARPUT, id, 'cot', means.cot
    NCDF_VARPUT, id, 'cot_liq', means.cot_liq
    NCDF_VARPUT, id, 'cot_ice', means.cot_ice
    NCDF_VARPUT, id, 'cer', means.cer
    NCDF_VARPUT, id, 'cer_liq', means.cer_liq
    NCDF_VARPUT, id, 'cer_ice', means.cer_ice
    NCDF_VARPUT, id, 'cwp', means.cwp
    NCDF_VARPUT, id, 'lwp', means.lwp
    NCDF_VARPUT, id, 'iwp', means.iwp
    NCDF_VARPUT, id, 'nobs', counts.ctp
    NCDF_VARPUT, id, 'nobs_lwp', counts.lwp
    NCDF_VARPUT, id, 'nobs_iwp', counts.iwp

    NCDF_CLOSE, id ;Close netCDF output file

    PRINT, '** CREATED: ', path_out + file_out

END
