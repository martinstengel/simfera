!-------------------------------------------------------------------------------
! Name: sim_ncdf.F90
!-------------------------------------------------------------------------------
MODULE SIM_NCDF 

    CONTAINS

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
        CALL GET_VARDIM_VARID( ncid, 'longitude', VarID, sdata%nlon )
        ALLOCATE( sdata%lon( sdata%nlon ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata%lon ) )

        ! latitude
        CALL GET_VARDIM_VARID( ncid, 'latitude', VarID, sdata%nlat )
        ALLOCATE( sdata%lat( sdata%nlat ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata%lat ) )

        ! sea surface temperature
        CALL CHECK( nf90_inq_varid( ncid, 'sst', VarID ) )
        ALLOCATE( sdata%sst2d( sdata%nlon, sdata%nlat ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata%sst2d ) )

        ! attributes
        CALL CHECK( nf90_get_att( ncid, VarID, 'scale_factor', scale_factor )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'add_offset', add_offset )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'missing_value', missing_value )  )
        CALL CHECK( nf90_get_att( ncid, VarID, '_FillValue', fill_value )  )
        
        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )


        PRINT*, "   Create Land/Sea mask: land = ",land," sea = ",sea 

        ALLOCATE( sdata%lsm2d( sdata%nlon, sdata%nlat ) )

        WHERE( sdata%sst2d == fill_value .OR. sdata%sst2d == missing_value ) 
            sdata%lsm2d = land
            sdata%sst2d = sint_fill_value
        ELSEWHERE
            sdata%lsm2d = sea
            sdata%sst2d = sdata%sst2d*scale_factor+add_offset
        END WHERE


        PRINT*, "   Create ERA-Interim 2D-grid" 

        ALLOCATE( sdata%lon2d( sdata%nlon, sdata%nlat ) )
        ALLOCATE( sdata%lat2d( sdata%nlon, sdata%nlat ) )
        
        ! each column of i-th row = longitude
        DO i=1, sdata%nlon
            sdata%lon2d(i,:) = sdata%lon(i)
        END DO
        ! each row of i-th column = latitude
        DO i=1, sdata%nlat
            sdata%lat2d(:,i) = sdata%lat(i)
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
        CALL GET_VARDIM_VARID( ncid, 'lon', VarID, idata%xdim )
        ALLOCATE( idata%lon( idata%xdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%lon ) )

        ! latitude
        CALL GET_VARDIM_VARID( ncid, 'lat', VarID, idata%ydim )
        ALLOCATE( idata%lat( idata%ydim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%lat ) )

        ! pressure levels
        CALL GET_VARDIM_VARID( ncid, 'lev', VarID, idata%zdim )
        ALLOCATE( idata%plevel( idata%zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%plevel ) )

        ! cloud cover
        CALL CHECK( nf90_inq_varid( ncid, 'var248', VarID ) )
        ALLOCATE( idata%cc( idata%xdim, idata%ydim, idata%zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%cc ) )

        ! liquid cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        CALL CHECK( nf90_inq_varid( ncid, 'var246', VarID ) )
        ALLOCATE( idata%lwc( idata%xdim, idata%ydim, idata%zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%lwc ) )

        ! ice cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        CALL CHECK( nf90_inq_varid( ncid, 'var247', VarID ) )
        ALLOCATE( idata%iwc( idata%xdim, idata%ydim, idata%zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%iwc ) )

        ! geopotential height [m2/s2]
        CALL CHECK( nf90_inq_varid( ncid, 'var129', VarID ) )
        ALLOCATE( idata%geop( idata%xdim, idata%ydim, idata%zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%geop ) )

        ! temperature [K]
        CALL CHECK( nf90_inq_varid( ncid, 'var130', VarID ) )
        ALLOCATE( idata%temp( idata%xdim, idata%ydim, idata%zdim ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%temp ) )

        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )

        ! allocate SZA2d array
        ALLOCATE( idata%sza2d( idata%xdim, idata%ydim ) )

        ! pressure difference
        idata%dpres=idata%plevel(2:SIZE(idata%plevel)) - &
                    idata%plevel(1:SIZE(idata%plevel)-1)
        
        ! split filename ! ERA_Interim_an_20080701_00+00_plev
        idx = INDEX( TRIM(ifile), filbase )

        idata%filename = TRIM( ifile(idx:LEN_TRIM(ifile)) )
        idata%dirname  = TRIM( ifile(1:idx-1) )
        idata%basename = TRIM( ifile(idx:SCAN(TRIM(ifile),'.')-1) )

        string = TRIM( ifile(idx+fb:idx+fb+3) )
        READ(string, '(I4)') idata%year

        string = TRIM( ifile(idx+fb+4:idx+fb+5) )
        READ(string, '(I2)') idata%month

        string = TRIM( ifile(idx+fb+6:idx+fb+7) )
        READ(string, '(I2)') idata%day

        string = TRIM( ifile(idx+fb+9:idx+fb+10) )
        READ(string, '(I2)') idata%hour

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

        CHARACTER(LEN=var_name) :: varname
        INTEGER, INTENT(INOUT)  :: fileid
        INTEGER, INTENT(OUT)    :: vardim, varid

        CALL CHECK( nf90_inq_dimid( fileid, varname, vardim ) )
        CALL CHECK( nf90_inq_varid( fileid, varname, varid ) )
        CALL CHECK( nf90_inquire_dimension( fileid, vardim, varname, varid ) )

    END SUBROUTINE GET_VARDIM_VARID

    !==========================================================================

END MODULE SIM_NCDF
