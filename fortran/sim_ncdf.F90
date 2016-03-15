!-------------------------------------------------------------------------------
! Name: sim_ncdf.F90
!-------------------------------------------------------------------------------
MODULE SIM_NCDF 

    CONTAINS

    !==========================================================================

    SUBROUTINE DEFINE_VAR( fid, flag, short, long, units, n_dim, varid )

        USE NETCDF
        IMPLICIT NONE

        INTEGER,           INTENT(IN)  :: fid, flag, n_dim
        CHARACTER(LEN=64), INTENT(IN)  :: short, long, units
        INTEGER,           INTENT(OUT) :: varid

        !local variable
        INTEGER :: dimid

        CALL CHECK ( nf90_def_dim( fid, short, n_dim, dimid ) )
        IF ( flag == 3 ) THEN 
            CALL CHECK ( nf90_def_var( fid, short, NF90_DOUBLE, dimid, varid ) )
        END IF
        CALL CHECK ( nf90_put_att( fid, varid, "units", units ) )
        CALL CHECK ( nf90_put_att( fid, varid, "long_name", long ) )

    END SUBROUTINE DEFINE_VAR

    !==========================================================================

    SUBROUTINE WRITE_MONTHLY_MEAN( aux, set, fin, cnt )

        USE COMMON_CONSTANTS
        USE CALENDER
        USE STRUCTS
        USE NETCDF

        IMPLICIT NONE

        TYPE(era_aux), INTENT(IN) :: aux
        TYPE(config),  INTENT(IN) :: set
        TYPE(l3_vars), INTENT(IN) :: fin
        TYPE(npoints), INTENT(IN) :: cnt

        ! local variables
        INTEGER                    :: ncid
        CHARACTER(LEN=path_length) :: nc_file
        CHARACTER(LEN=7)           :: thv_int
        CHARACTER(LEN=1)           :: scops_int, mpc_int
        CHARACTER(LEN=20)          :: scops_def, mpc_def
        DOUBLE PRECISION           :: julday, ref_julday, itime

        ! dimensions
        INTEGER(KIND=sint), PARAMETER :: ref_year = 1970
        INTEGER(KIND=sint), PARAMETER :: ref_month = 1
        INTEGER(KIND=sint), PARAMETER :: ref_day = 1
        INTEGER(KIND=lint), PARAMETER :: DIM_TIM = 1
        INTEGER(KIND=lint)  :: DIM_LAT, DIM_LON, n_dim
        INTEGER(KIND=lint)  :: tim_dimid, lat_dimid, lon_dimid
        INTEGER(KIND=lint)  :: tim_varid, lat_varid, lon_varid
        INTEGER(KIND=lint)  :: phase_varid
        INTEGER(KIND=lint)  :: cot2d_cen_varid, ctp2d_cen_varid
        INTEGER(KIND=lint)  :: cot2d_bor_varid, ctp2d_bor_varid
        INTEGER(KIND=lint)  :: dimid, flag
        CHARACTER(LEN=64)   :: short, long

        PRINT*, "** WRITE_MONTHLY_MEAN"

        ! scops and mpc type
        IF ( set % scops == 1 ) scops_def = "random"
        IF ( set % scops == 2 ) scops_def = "max/random"
        IF ( set % mpc == 1 ) mpc_def = "separated phase"
        IF ( set % mpc == 2 ) mpc_def = "mixed phase"

        ! grid dimensions
        DIM_LON = aux % nlon
        DIM_LAT = aux % nlat

        ! time dimension
        CALL GREG2JD ( ref_year, ref_month, ref_day, ref_julday )
        CALL GREG2JD ( set % sy, set % sm, set % sd, julday )
        itime = julday - ref_julday

        ! convert number to character
        WRITE(thv_int,   '(F4.2)') set % thv
        WRITE(scops_int, '(I1)')   set % scops
        WRITE(mpc_int,   '(I1)')   set % mpc

        ! create output nc-filename
        nc_file = TRIM(set % out_path) // "/ERA_Interim_MM" // &
                  set % start_date(1:6) // &
                  '_cot-thv-' // TRIM(thv_int) // &
                  '_scops-' // scops_int // &
                  '_mpc-' // mpc_int // '.nc' 

        PRINT*, "   nc-file: ", TRIM(nc_file)


        ! create ncdf file
        CALL CHECK ( nf90_create( TRIM(nc_file), nf90_clobber, ncid ) )

        ! global attributes
        CALL CHECK ( nf90_put_att( ncid, NF90_GLOBAL, "Source", "ERA-Interim" ) )
        CALL CHECK ( nf90_put_att( ncid, NF90_GLOBAL, "TIME_COVERAGE_START", set % start_date(1:6) ) )
        CALL CHECK ( nf90_put_att( ncid, NF90_GLOBAL, "TIME_COVERAGE_RESOLUTION", "P1M" ) )
        CALL CHECK ( nf90_put_att( ncid, NF90_GLOBAL, "cot_thv", set % thv ) )
        CALL CHECK ( nf90_put_att( ncid, NF90_GLOBAL, "number_of_files", cnt % file_counter ) )
        CALL CHECK ( nf90_put_att( ncid, NF90_GLOBAL, "scops", scops_def ) )
        CALL CHECK ( nf90_put_att( ncid, NF90_GLOBAL, "mpc", mpc_def ) )

        ! define dimensions
        CALL CHECK ( nf90_def_dim( ncid, "time", DIM_TIM, tim_dimid ) )
        CALL CHECK ( nf90_def_dim( ncid, "lon",  DIM_LON, lon_dimid ) )
        CALL CHECK ( nf90_def_dim( ncid, "lat",  DIM_LAT, lat_dimid ) )

        ! define coordinate variables
        CALL CHECK ( nf90_def_var( ncid, "time", NF90_DOUBLE, tim_dimid, tim_varid ) )
        CALL CHECK ( nf90_def_var( ncid, "lon",  NF90_REAL,   lon_dimid, lon_varid ) )
        CALL CHECK ( nf90_def_var( ncid, "lat",  NF90_REAL,   lat_dimid, lat_varid ) )

        ! assign units attributes
        CALL CHECK ( nf90_put_att( ncid, lon_varid, "units", "degrees_east" ) )
        CALL CHECK ( nf90_put_att( ncid, lat_varid, "units", "degrees_north" ) )

        ! long_names
        CALL CHECK ( nf90_put_att( ncid, lon_varid, "long_name", "longitude" ) )
        CALL CHECK ( nf90_put_att( ncid, lat_varid, "long_name", "latitude" ) )


        !flag = 1 !long integer
        !flag = 2 !float
        !flag = 3 !double

        ! phase definition
        flag  = 3
        short = "hist_phase"
        long  = "phase histogram bins"
        n_dim = n_hist_phase
        CALL DEFINE_VAR( ncid, flag, short, long, " ", n_dim, phase_varid )


        !! 2d histogram definitions
        !short = "hist2d_cot_bin_border"
        !long  = "cot histogram border values"
        !n_dim = SIZE(set % hist_cot_2d_axis)
        !CALL CHECK ( nf90_def_dim( ncid, short, n_dim, dimid ) )
        !CALL CHECK ( nf90_def_var( ncid, short, NF90_DOUBLE, dimid, cot2d_bor_varid ) )
        !CALL CHECK ( nf90_put_att( ncid, cot2d_bor_varid, "units", " " ) )
        !CALL CHECK ( nf90_put_att( ncid, cot2d_bor_varid, "long_name", long ) )

        !short = "hist2d_cot_bin_centre"
        !long  = "cot histogram bins"
        !n_dim = SIZE(set % hist_cot_2d_bin)
        !CALL CHECK ( nf90_def_dim( ncid, short, n_dim, dimid ) )
        !CALL CHECK ( nf90_def_var( ncid, short, NF90_DOUBLE, dimid, cot2d_cen_varid ) )
        !CALL CHECK ( nf90_put_att( ncid, cot2d_cen_varid, "units", " " ) )
        !CALL CHECK ( nf90_put_att( ncid, cot2d_cen_varid, "long_name", long ) )

        !short = "hist2d_ctp_bin_border"
        !long  = "ctp histogram border values"
        !n_dim = SIZE(set % hist_ctp_2d_axis)
        !CALL CHECK ( nf90_def_dim( ncid, short, n_dim, dimid ) )
        !CALL CHECK ( nf90_def_var( ncid, short, NF90_DOUBLE, dimid, ctp2d_bor_varid ) )
        !CALL CHECK ( nf90_put_att( ncid, ctp2d_bor_varid, "units", " " ) )
        !CALL CHECK ( nf90_put_att( ncid, ctp2d_bor_varid, "long_name", long ) )

        !short = "hist2d_ctp_bin_centre"
        !long  = "ctp histogram bins"
        !n_dim = SIZE(set % hist_ctp_2d_bin)
        !CALL CHECK ( nf90_def_dim( ncid, short, n_dim, dimid ) )
        !CALL CHECK ( nf90_def_var( ncid, short, NF90_DOUBLE, dimid, ctp2d_cen_varid ) )
        !CALL CHECK ( nf90_put_att( ncid, ctp2d_cen_varid, "units", " " ) )
        !CALL CHECK ( nf90_put_att( ncid, ctp2d_cen_varid, "long_name", long ) )


        !! 1d histogram definitions
        !short = "hist1d_cot_bin_border"
        !long  = "histogram_cot1d"
        !n_dim = SIZE(set % hist_cot_1d_axis)
        !CALL CHECK ( nf90_def_dim( ncid, short, n_dim, dimid ) )
        !CALL CHECK ( nf90_def_var( ncid, short, NF90_DOUBLE, dimid, cot1d_bor_varid ) )
        !CALL CHECK ( nf90_put_att( ncid, cot1d_bor_varid, "units", " " ) )
        !CALL CHECK ( nf90_put_att( ncid, cot1d_bor_varid, "long_name", long ) )

        !short = "hist1d_cot_bin_centre"
        !long  = "histogram_cot_bin1d"
        !n_dim = SIZE(set % hist_cot_1d_bin)
        !CALL CHECK ( nf90_def_dim( ncid, short, n_dim, dimid ) )
        !CALL CHECK ( nf90_def_var( ncid, short, NF90_DOUBLE, dimid, cot1d_cen_varid ) )
        !CALL CHECK ( nf90_put_att( ncid, cot1d_cen_varid, "units", " " ) )
        !CALL CHECK ( nf90_put_att( ncid, cot1d_cen_varid, "long_name", long ) )

        !REAL(KIND=sreal) :: hist_ctp_1d_axis(n_ctp_bins+1)
        !REAL(KIND=sreal) :: hist_ctp_1d_bin(n_ctp_bins)

        !REAL(KIND=sreal) :: hist_ctt_1d_axis(n_ctt_bins+1)
        !REAL(KIND=sreal) :: hist_ctt_1d_bin(n_ctt_bins)

        !REAL(KIND=sreal) :: hist_cer_1d_axis(n_cer_bins+1)
        !REAL(KIND=sreal) :: hist_cer_1d_bin(n_cer_bins)

        !REAL(KIND=sreal) :: hist_cwp_1d_axis(n_cwp_bins+1)
        !REAL(KIND=sreal) :: hist_cwp_1d_bin(n_cwp_bins)

        ! end define mode
        CALL CHECK ( nf90_enddef(ncid) )


        ! write coordinate variables
        CALL CHECK ( nf90_put_var(ncid, tim_varid, itime) )
        CALL CHECK ( nf90_put_var(ncid, lon_varid, aux % lon) )
        CALL CHECK ( nf90_put_var(ncid, lat_varid, aux % lat) )

        ! write histogram variables
        CALL CHECK ( nf90_put_var(ncid, phase_varid, set % hist_phase) )
        !CALL CHECK ( nf90_put_var(ncid, cot2d_bor_varid, set % hist_cot_2d_axis) )
        !CALL CHECK ( nf90_put_var(ncid, cot2d_cen_varid, set % hist_cot_2d_bin) )
        !CALL CHECK ( nf90_put_var(ncid, ctp2d_bor_varid, set % hist_ctp_2d_axis) )
        !CALL CHECK ( nf90_put_var(ncid, ctp2d_cen_varid, set % hist_ctp_2d_bin) )

        ! write netcdf variables

        ! close ncdf file
        CALL CHECK( nf90_close(  ncid ) )

        stop

    END SUBROUTINE WRITE_MONTHLY_MEAN

    !==========================================================================

    SUBROUTINE READ_AUX_DATA( sfile, sdata )

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE NETCDF

        IMPLICIT NONE

        INTEGER            :: ncid, DimID, VarID, VarDim, i 
        REAL(KIND=sreal)   :: scale_factor, add_offset
        INTEGER(KIND=lint) :: fill_value, missing_value
        CHARACTER(LEN=file_length), INTENT(IN) :: sfile
        TYPE(era_aux), INTENT(INOUT)           :: sdata

        PRINT*, "** READ_AUX_DATA"

        PRINT*, "   Read SST_FILE "//TRIM(sfile)

        ! open ncdf file
        CALL CHECK( nf90_open( sfile, nf90_nowrite, ncid) )

        ! longitude
        CALL GET_VARDIM_VARID( ncid, 'longitude', VarID, sdata % nlon )
        ALLOCATE( sdata % lon( sdata % nlon ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata % lon ) )

        ! latitude
        CALL GET_VARDIM_VARID( ncid, 'latitude', VarID, sdata % nlat )
        ALLOCATE( sdata % lat( sdata % nlat ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata % lat ) )

        ! sea surface temperature
        CALL CHECK( nf90_inq_varid( ncid, 'sst', VarID ) )
        ALLOCATE( sdata % sst2d( sdata % nlon, sdata % nlat ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata % sst2d ) )

        ! attributes
        CALL CHECK( nf90_get_att( ncid, VarID, 'scale_factor', scale_factor )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'add_offset', add_offset )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'missing_value', missing_value )  )
        CALL CHECK( nf90_get_att( ncid, VarID, '_FillValue', fill_value )  )
        
        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )


        PRINT*, "   Create Land/Sea mask: land = ",land," sea = ",sea 

        ALLOCATE( sdata % lsm2d( sdata % nlon, sdata % nlat ) )

        WHERE( sdata % sst2d == fill_value .OR. sdata % sst2d == missing_value ) 
            sdata % lsm2d = land
            sdata % sst2d = sint_fill_value
        ELSEWHERE
            sdata % lsm2d = sea
            sdata % sst2d = sdata % sst2d*scale_factor+add_offset
        END WHERE


        PRINT*, "   Create ERA-Interim 2D-grid" 

        ALLOCATE( sdata % lon2d( sdata % nlon, sdata % nlat ) )
        ALLOCATE( sdata % lat2d( sdata % nlon, sdata % nlat ) )
        
        ! each column of i-th row = longitude
        DO i=1, sdata % nlon
            sdata % lon2d(i,:) = sdata % lon(i)
        END DO
        ! each row of i-th column = latitude
        DO i=1, sdata % nlat
            sdata % lat2d(:,i) = sdata % lat(i)
        END DO

    END SUBROUTINE READ_AUX_DATA

    !==========================================================================

    SUBROUTINE READ_ERA_NCFILE( ifile, idata )

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE NETCDF

        IMPLICIT NONE

        INTEGER(KIND=sint), PARAMETER :: fb=15
        CHARACTER(LEN=fb),  PARAMETER :: filbase="ERA_Interim_an_"
        INTEGER                       :: ncid, DimID, VarID, VarDim, idx
        CHARACTER(LEN=20)             :: string
        CHARACTER(LEN=file_length), INTENT(IN) :: ifile
        TYPE(era_input), INTENT(INOUT)         :: idata

        PRINT*, "** READ_ERA_NCFILE"

        ! open ncdf file
        CALL CHECK( nf90_open( ifile, nf90_nowrite, ncid) )

        ! longitude
        CALL GET_VARDIM_VARID( ncid, 'lon', VarID, idata % xdim )
        ALLOCATE( idata % lon( idata % xdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lon ) )

        ! latitude
        CALL GET_VARDIM_VARID( ncid, 'lat', VarID, idata % ydim )
        ALLOCATE( idata % lat( idata % ydim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lat ) )

        ! pressure levels
        CALL GET_VARDIM_VARID( ncid, 'lev', VarID, idata % zdim )
        ALLOCATE( idata % plevel( idata % zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % plevel ) )

        ! cloud cover
        CALL CHECK( nf90_inq_varid( ncid, 'var248', VarID ) )
        ALLOCATE( idata % cc( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % cc ) )

        ! liquid cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        CALL CHECK( nf90_inq_varid( ncid, 'var246', VarID ) )
        ALLOCATE( idata % lwc( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % lwc ) )

        ! ice cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        CALL CHECK( nf90_inq_varid( ncid, 'var247', VarID ) )
        ALLOCATE( idata % iwc( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % iwc ) )

        ! geopotential height [m2/s2]
        CALL CHECK( nf90_inq_varid( ncid, 'var129', VarID ) )
        ALLOCATE( idata % geop( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % geop ) )

        ! temperature [K]
        CALL CHECK( nf90_inq_varid( ncid, 'var130', VarID ) )
        ALLOCATE( idata % temp( idata % xdim, idata % ydim, idata % zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata % temp ) )

        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )


        ! allocate SZA2d array
        ALLOCATE( idata % sza2d( idata % xdim, idata % ydim ) )


        ! negative values set to zero
        WHERE ( idata % cc  .LT. 0.0 ) idata % cc  = 0.0
        WHERE ( idata % lwc .LT. 0.0 ) idata % lwc = 0.0
        WHERE ( idata % iwc .LT. 0.0 ) idata % iwc = 0.0


        ! pressure difference
        idata % dpres = idata % plevel(2:SIZE(idata % plevel)) - &
                        idata % plevel(1:SIZE(idata % plevel)-1)
        

        ! split filename ! ERA_Interim_an_20080701_00+00_plev
        idx = INDEX( TRIM(ifile), filbase )

        idata % filename = TRIM( ifile(idx:LEN_TRIM(ifile)) )
        idata % dirname  = TRIM( ifile(1:idx-1) )
        idata % basename = TRIM( ifile(idx:SCAN(TRIM(ifile),'.')-1) )

        string = TRIM( ifile(idx+fb:idx+fb+3) )
        READ(string, '(I4)') idata % year

        string = TRIM( ifile(idx+fb+4:idx+fb+5) )
        READ(string, '(I2)') idata % month

        string = TRIM( ifile(idx+fb+6:idx+fb+7) )
        READ(string, '(I2)') idata % day

        string = TRIM( ifile(idx+fb+9:idx+fb+10) )
        READ(string, '(I2)') idata % hour

    END SUBROUTINE READ_ERA_NCFILE

    !==========================================================================

    SUBROUTINE CHECK( status )

        USE NETCDF

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: status

        IF (status /= nf90_noerr) THEN
            PRINT *, TRIM(nf90_strerror(status))
            STOP "Stopped"
        END IF

    END SUBROUTINE CHECK

    !==========================================================================

    SUBROUTINE GET_VARDIM_VARID( fileid, varname, vardim, varid )

        USE COMMON_CONSTANTS
        USE NETCDF

        IMPLICIT NONE

        CHARACTER(LEN=64)       :: varname
        INTEGER, INTENT(INOUT)  :: fileid
        INTEGER, INTENT(OUT)    :: vardim, varid

        CALL CHECK( nf90_inq_dimid( fileid, varname, vardim ) )
        CALL CHECK( nf90_inq_varid( fileid, varname, varid ) )
        CALL CHECK( nf90_inquire_dimension( fileid, vardim, varname, varid ) )

    END SUBROUTINE GET_VARDIM_VARID

    !==========================================================================

END MODULE SIM_NCDF
