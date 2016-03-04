!-------------------------------------------------------------------------------
! Name: subs.F90
!-------------------------------------------------------------------------------
MODULE SUBS 

    CONTAINS

    !==========================================================================
    ! BASH stuff
    !==========================================================================
    SUBROUTINE GET_FILE_LIST(cfg, year, month, file_list)

        USE STRUCTS
        USE COMMON_CONSTANTS
    
        IMPLICIT NONE
    
        REAL                       :: r
        CHARACTER(LEN=4)           :: ystr
        CHARACTER(LEN=2)           :: mstr
        INTEGER(KIND=sint)         :: i, reason, nfiles, ilen
        CHARACTER(LEN=file_length) :: command, txtfile, inppath, line
    
        TYPE(config), INTENT(IN)                             :: cfg
        INTEGER(KIND=sint), INTENT(IN)                       :: year, month
        CHARACTER(LEN=file_length), ALLOCATABLE, INTENT(OUT) :: file_list(:,:)
    
        ! convert integer to character
        WRITE(ystr,'(I4)') year
        WRITE(mstr,'(I0.2)') month
    
        ! get the files
        txtfile = TRIM(cfg%out_path)//'/'//&
                  ystr//mstr//'_file_list.txt'
        inppath = TRIM(cfg%inp_path)//'/'//ystr//mstr
        command = 'ls '//TRIM(inppath)//'/ERA*plev > '//&
                  TRIM(txtfile)
        CALL system(command)
    
        !how many files
        OPEN(31, FILE=TRIM(txtfile), ACTION="read")
        i=0
        DO
            READ(31,FMT='(A)', IOSTAT=reason) r
            IF (reason/=0) EXIT
            i = i + 1
        END DO
    
        nfiles = i
        ALLOCATE(file_list(nfiles,1))
    
        REWIND(31)
        DO i = 1, nfiles
            READ(31,"(A)") line
            ilen = LEN_TRIM(line)
            READ(TRIM(line(1:ilen)),'(A)') file_list(i,1)
        END DO
    
        CLOSE(31)

        PRINT*, "** ",nfiles," ERA-I files for ", year, month
    
    END SUBROUTINE GET_FILE_LIST

    !==========================================================================

    SUBROUTINE CONVERT_ERA_FILE( ifile, ofile )
    
        USE COMMON_CONSTANTS
        IMPLICIT NONE
    
        LOGICAL                                 :: file_exists
        CHARACTER(LEN=900)                      :: command
        CHARACTER(LEN=file_length), INTENT(IN)  :: ifile
        CHARACTER(LEN=file_length), INTENT(OUT) :: ofile
    
        ofile = TRIM(ifile)//'.nc'
        INQUIRE( FILE=TRIM(ofile), EXIST=file_exists )
    
        IF (file_exists) THEN 
            WRITE (*,*) "** ERA ncfile already exists: '"//TRIM(ofile)//"'"
        ELSE
            command = "cdo -f nc copy "//TRIM(ifile)//" "//TRIM(ofile)
            WRITE(*,'(a)') "** "//TRIM(command)
            CALL system( command )
        ENDIF
    
    END SUBROUTINE CONVERT_ERA_FILE

    !==========================================================================

    SUBROUTINE CREATE_DIR( newDirPath )
    
        USE COMMON_CONSTANTS
    
        IMPLICIT NONE
    
        LOGICAL                                :: dirExists
        CHARACTER(LEN=256)                     :: mkdirCmd
        CHARACTER(LEN=path_length), INTENT(IN) :: newDirPath
    
        ! Check if the directory exists first
        ! Works with ifort, but not gfortran
        !INQUIRE( directory=newDirPath, EXIST=dirExists ) 
        ! Works with gfortran, but not ifort
        INQUIRE( FILE=TRIM(newDirPath)//'/.', EXIST=dirExists )  
    
        IF (dirExists) THEN 
            WRITE (*,*) "** Directory already exists: '"//&
                        TRIM(newDirPath)//"'"
        ELSE
            mkdirCmd = 'mkdir -p '//TRIM(newDirPath)
            WRITE(*,'(a)') "** Creating new directory: '"//&
                           TRIM(mkdirCmd)//"'"
            CALL system( mkdirCmd )
        ENDIF
    
    END SUBROUTINE CREATE_DIR

    !==========================================================================
    
    
    
    !==========================================================================
    SUBROUTINE READ_CONFIG(cfg)
    !==========================================================================
    
        USE COMMON_CONSTANTS
        USE STRUCTS
        USE FUNCS, only: NUMBER_OF_DAYS
    
        IMPLICIT NONE
    
        INTEGER(KIND=sint)          :: io, idx, lun, ilen
        CHARACTER(LEN=4)            :: year
        CHARACTER(LEN=2)            :: month, day
        CHARACTER(LEN=500)          :: line, what
        TYPE(config), INTENT(INOUT) :: cfg
    
        OPEN(NEWUNIT=lun, FILE="config.file", STATUS="old")
        DO
            READ(lun,"(a)",IOSTAT=io) line
            IF (io>0) THEN
                WRITE(*,*) "Check input. Something was wrong."
                EXIT
            ELSE IF (io<0) THEN
                EXIT
            ELSE
                IF ((line(1:1) == '#') .OR. (line(1:1) == ';') .OR. &
                    (LEN_TRIM(line) == 0) ) THEN
                    CYCLE
                ELSE
                    idx = SCAN(line,"=")
                    what = TRIM(line(:idx-1))
                    ilen = LEN_TRIM(line)
    
                    IF (TRIM(what) == "THV") THEN 
                        READ(line(idx+1:ilen),'(F8.2)') cfg%thv
    
                    ELSEIF (TRIM(what) == "MPC") THEN
                        READ(line(idx+1:ilen),'(I1)') cfg%mpc
    
                    ELSEIF (TRIM(what) == "SCOPS") THEN
                        READ(line(idx+1:ilen),'(I1)') cfg%scops
    
                    ELSEIF (TRIM(what) == "OUT_PWD") THEN
                        READ(TRIM(line(idx+1:ilen)),'(A)') cfg%out_path
    
                    ELSEIF (TRIM(what) == "INP_PWD") THEN
                        READ(TRIM(line(idx+1:ilen)),'(A)') cfg%inp_path
    
                    ELSEIF (TRIM(what) == "REP_PWD") THEN
                        READ(TRIM(line(idx+1:ilen)),'(A)') cfg%rep_path
    
                    ELSEIF (TRIM(what) == "SSTFILE") THEN
                        READ(TRIM(line(idx+1:ilen)),'(A)') cfg%sst_file
    
                    ELSEIF (TRIM(what) == "STARTYEAR") THEN
                        READ(line(idx+1:ilen),'(I4)') cfg%sy
    
                    ELSEIF (TRIM(what) == "STOPYEAR") THEN
                        READ(line(idx+1:ilen),'(I4)') cfg%ey
    
                    ELSEIF (TRIM(what) == "STARTMONTH") THEN
                        READ(line(idx+1:ilen),'(I2)') cfg%sm
    
                    ELSEIF (TRIM(what) == "STOPMONTH") THEN
                        READ(line(idx+1:ilen),'(I2)') cfg%em
    
                    ELSEIF (TRIM(what) == "STARTDAY") THEN
                        READ(line(idx+1:ilen),'(I2)') cfg%sd
    
                    ELSEIF (TRIM(what) == "STOPDAY") THEN
                        READ(line(idx+1:ilen),'(I2)') cfg%ed
    
                    ELSE
                        PRINT*, "NOT defined in config.file"
                    END IF
    
                END IF
            END IF
        END DO
        CLOSE(lun)
    
    
        ! convert character to integer
        ! READ(character, '(I2)') integer
    
        ! get last day of month if ed=0
        IF (cfg%ed == 0) cfg%ed = NUMBER_OF_DAYS(cfg%em,cfg%ed)
    
        ! convert integer to character
        WRITE(year,'(I4)') cfg%sy
        WRITE(month,'(I0.2)') cfg%sm
        WRITE(day,'(I0.2)') cfg%sd
        cfg%start_date = year//month//day
    
        WRITE(year,'(I4)') cfg%ey
        WRITE(month,'(I0.2)') cfg%em
        WRITE(day,'(I0.2)') cfg%ed
        cfg%end_date = year//month//day
    
    
        ! set histogram definitions
    
        ! 2d histogram
        cfg%hist_cot=(/0.0, 0.3, 0.6, 1.3, 2.2, 3.6, 5.8, &
                       9.4, 15.0, 23.0, 41.0, 60.0, 80.0, 100./)
        cfg%hist_cot_bin=cfg%hist_cot(1:n_hist_cot-1)*0.5 + &
                         cfg%hist_cot(2:n_hist_cot)*0.5
    
        cfg%hist_ctp=(/1.0, 90.0, 180.0, 245.0, 310.0, 375.0, &
                       440.0, 500.0, 560.0, 620.0, 680.0, &
                       740.0, 800.0, 875.0, 950.0, 1100./)
        cfg%hist_ctp_bin=cfg%hist_ctp(1:n_hist_ctp-1)*0.5 + &
                         cfg%hist_ctp(2:n_hist_ctp)*0.5
    
        ! 1d histogram
    
        cfg%hist_cot_1d_axis=(/0.0, 0.3, 0.6, 1.3, 2.2, 3.6, 5.8, &
                               9.4, 15.0, 23.0, 41.0, 60.0, 80.0, 100. /) 
        cfg%hist_cot_1d_bin=cfg%hist_cot_1d_axis(1:n_cot_bins)*0.5 + &
                            cfg%hist_cot_1d_axis(2:n_cot_bins+1)*0.5
    
        cfg%hist_ctp_1d_axis=(/ 1.0, 90.0, 180.0, 245.0, 310.0, 375.0, &
                                440.0, 500.0, 560.0, 620.0, 680.0, 740.0, 800.0, &
                                875.0, 950.0, 1100.0 /)
        cfg%hist_ctp_1d_bin=cfg%hist_ctp_1d_axis(1:n_ctp_bins)*0.5 + &
                            cfg%hist_ctp_1d_axis(2:n_ctp_bins+1)*0.5
    
        cfg%hist_ctt_1d_axis=(/ 200.0, 210.0, 220.0, 230.0, 235.0, &
                                240.0, 245.0, 250.0, 255.0, 260.0, 265.0, &
                                270.0, 280.0, 290.0, 300.0, 310.0, 350.0 /)
        cfg%hist_ctt_1d_bin=cfg%hist_ctt_1d_axis(1:n_ctt_bins)*0.5 + & 
                            cfg%hist_ctt_1d_axis(2:n_ctt_bins+1)*0.5
    
        cfg%hist_cwp_1d_axis=(/ 0.0, 5.0, 10.0, 20.0, 35.0, 50.0, &
                                75.0, 100.0, 150.0, 200.0, 300.0, 500.0, &
                                1000.0, 2000.0, 100000.0 /)
        cfg%hist_cwp_1d_bin=cfg%hist_cwp_1d_axis(1:n_cwp_bins)*0.5 + &
                            cfg%hist_cwp_1d_axis(2:n_cwp_bins+1)*0.5
    
        cfg%hist_cer_1d_axis=(/ 0.0, 3.0, 6.0, 9.0, 12.0, 15.0, &
                                20.0, 25.0, 30.0, 40.0, 60.0, 80.0 /)
        cfg%hist_cer_1d_bin=cfg%hist_cer_1d_axis(1:n_cer_bins)*0.5 + &
                            cfg%hist_cer_1d_axis(2:n_cer_bins+1)*0.5
    
        PRINT*, ''
        PRINT*, '** CONFIG SETTINGS '
        PRINT('(A13, E8.2)'), "COT-THV: ", cfg%thv
        PRINT('(A13, I1)'), "MPC: ", cfg%mpc
        PRINT('(A13, I1)'), "SCOPS: ", cfg%scops
        PRINT('(A13, A8)'), "START: ", cfg%start_date
        PRINT('(A13, A8)'), "STOP: ", cfg%end_date
        PRINT('(A13, A)'), "REP_PWD: ", TRIM(cfg%rep_path)
        PRINT('(A13, A)'), "INP_PWD: ", TRIM(cfg%inp_path)
        PRINT('(A13, A)'), "OUT_PWD: ", TRIM(cfg%out_path)
        PRINT('(A13, A)'), "SST_FILE: ", TRIM(cfg%sst_file)
        PRINT*, ''

    !==========================================================================
    END SUBROUTINE READ_CONFIG
    !==========================================================================



    !==========================================================================
    ! NCDF stuff
    !==========================================================================
    SUBROUTINE READ_AUX_DATA( sfile, sdata )

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE NETCDF

        IMPLICIT NONE

        INTEGER            :: ncid, DimID, VarID, VarDim, i, j
        REAL(KIND=sreal)   :: scale_factor, add_offset
        INTEGER(KIND=lint) :: fill_value, missing_value
        CHARACTER(LEN=file_length), INTENT(IN) :: sfile
        TYPE(era_sst_lsm), INTENT(INOUT)       :: sdata

        PRINT*, "** READ SST FILE and GET LAND/SEA MASK"

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
        ALLOCATE( sdata%sst( sdata%nlon, sdata%nlat ) )
        CALL CHECK( nf90_get_var( ncid, VarID, sdata%sst ) )

        ! attributes
        CALL CHECK( nf90_get_att( ncid, VarID, 'scale_factor', scale_factor )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'add_offset', add_offset )  )
        CALL CHECK( nf90_get_att( ncid, VarID, 'missing_value', missing_value )  )
        CALL CHECK( nf90_get_att( ncid, VarID, '_FillValue', fill_value )  )
        
        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )

        ! get lsm and scale sst and add offset
        ALLOCATE( sdata%lsm( sdata%nlon, sdata%nlat ) )

        WHERE( sdata%sst == fill_value .OR. sdata%sst == missing_value ) 
            sdata%lsm = land
            sdata%sst = sint_fill_value
        ELSEWHERE
            sdata%lsm = sea
            sdata%sst = sdata%sst*scale_factor+add_offset
        END WHERE

        !print*, minval(sdata%sst, mask=sdata%sst .NE. sint_fill_value)
        !print*, maxval(sdata%sst, mask=sdata%sst .NE. sint_fill_value)

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

        ! open ncdf file
        CALL CHECK( nf90_open( ifile, nf90_nowrite, ncid) )

        ! longitude
        CALL GET_VARDIM_VARID( ncid, 'lon', VarID, idata%nlon )
        ALLOCATE( idata%lon( idata%nlon ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%lon ) )

        ! latitude
        CALL GET_VARDIM_VARID( ncid, 'lat', VarID, idata%nlat )
        ALLOCATE( idata%lat( idata%nlat ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%lat ) )

        ! pressure levels
        CALL GET_VARDIM_VARID( ncid, 'lev', VarID, idata%nlev )
        ALLOCATE( idata%plevel( idata%nlev ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%plevel ) )

        ! cloud cover
        CALL CHECK( nf90_inq_varid( ncid, 'var248', VarID ) )
        ALLOCATE( idata%cc( idata%nlon, idata%nlat, idata%nlev ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%cc ) )

        ! liquid cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        CALL CHECK( nf90_inq_varid( ncid, 'var246', VarID ) )
        ALLOCATE( idata%lwc( idata%nlon, idata%nlat, idata%nlev ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%lwc ) )

        ! ice cloud water content [kg kg**-1] 
        ! i.e., [mass of condensate / mass of moist air]
        CALL CHECK( nf90_inq_varid( ncid, 'var247', VarID ) )
        ALLOCATE( idata%iwc( idata%nlon, idata%nlat, idata%nlev ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%iwc ) )

        ! geopotential height [m2/s2]
        CALL CHECK( nf90_inq_varid( ncid, 'var129', VarID ) )
        ALLOCATE( idata%geop( idata%nlon, idata%nlat, idata%nlev ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%geop ) )

        ! temperature [K]
        CALL CHECK( nf90_inq_varid( ncid, 'var130', VarID ) )
        ALLOCATE( idata%temp( idata%nlon, idata%nlat, idata%nlev ) )
        CALL CHECK( nf90_get_var( ncid, VarID, idata%temp ) )

        ! close ncdf file
        CALL CHECK( nf90_close( ncid ) )

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



    !==========================================================================
    ! DEALLOCATE ARRAYS
    !==========================================================================
    SUBROUTINE DEALLOCATE_INPUT( input )

        USE STRUCTS

        IMPLICIT NONE

        TYPE(era_input), INTENT(INOUT) :: input

        IF (ALLOCATED(input%lon))    DEALLOCATE(input%lon)
        IF (ALLOCATED(input%lat))    DEALLOCATE(input%lat)
        IF (ALLOCATED(input%plevel)) DEALLOCATE(input%plevel)
        IF (ALLOCATED(input%dpres))  DEALLOCATE(input%dpres)
        IF (ALLOCATED(input%cc))     DEALLOCATE(input%cc)
        IF (ALLOCATED(input%lwc))    DEALLOCATE(input%lwc)
        IF (ALLOCATED(input%iwc))    DEALLOCATE(input%iwc)
        IF (ALLOCATED(input%geop))   DEALLOCATE(input%geop)
        IF (ALLOCATED(input%temp))   DEALLOCATE(input%temp)

    END SUBROUTINE DEALLOCATE_INPUT
    !==========================================================================
    SUBROUTINE DEALLOCATE_AUX( aux )

        USE STRUCTS

        IMPLICIT NONE

        TYPE(era_sst_lsm), INTENT(INOUT) :: aux

        IF (ALLOCATED(aux%lon)) DEALLOCATE(aux%lon)
        IF (ALLOCATED(aux%lat)) DEALLOCATE(aux%lat)
        IF (ALLOCATED(aux%sst)) DEALLOCATE(aux%sst)
        IF (ALLOCATED(aux%lsm)) DEALLOCATE(aux%lsm)

    END SUBROUTINE DEALLOCATE_AUX
    !==========================================================================


END MODULE SUBS
