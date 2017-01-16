!===============================================================================
! Name: cloud_simulator.F90
!===============================================================================
PROGRAM CLOUD_SIMULATOR
!=============================================================================== 

    USE COMMON_CONSTANTS
    USE INITIALIZE
    USE UNDEFINE
    USE STRUCTS
    USE SIM_NCDF
    USE SIM_CORE
    USE SUBS

    IMPLICIT NONE

    ! local variables
    INTEGER(KIND=sint)  :: year, ystep=1, month, mstep=1
    INTEGER(KIND=sint)  :: ff, nfiles

    TYPE(config)        :: cfg
    TYPE(era_input)     :: input
    TYPE(era_aux)       :: aux
    TYPE(tmp_arrays)    :: temps
    TYPE(l3_vars)       :: final
    TYPE(npoints)       :: counts

    CHARACTER(LEN=10)   :: thv_str
    CHARACTER(LEN=1)    :: mpc_str, scops_str, overlap_str
    CHARACTER(LEN=1)    :: cwc_mod_str
    CHARACTER(LEN=4)    :: sy_str, ey_str
    CHARACTER(LEN=2)    :: sm_str, em_str, sd_str, ed_str

    CHARACTER(LEN=file_length)              :: ncfile, ts
    CHARACTER(LEN=file_length), ALLOCATABLE :: files(:,:)

    INTEGER :: x, y, nargs
    REAL    :: start, finish, start_month, finish_month

    PRINT*, ""
    PRINT*, "** cloud_simulator started"
    PRINT*, ""

    ! get number of arguments
    nargs = COMMAND_ARGUMENT_COUNT()
    IF ( nargs == 14 ) THEN
        CALL GET_COMMAND_ARGUMENT(  1, thv_str )
        CALL GET_COMMAND_ARGUMENT(  2, scops_str )
        CALL GET_COMMAND_ARGUMENT(  3, overlap_str )
        CALL GET_COMMAND_ARGUMENT(  4, mpc_str )
        CALL GET_COMMAND_ARGUMENT(  5, cwc_mod_str )
        CALL GET_COMMAND_ARGUMENT(  6, cfg % sst_file )
        CALL GET_COMMAND_ARGUMENT(  7, cfg % inp_path )
        CALL GET_COMMAND_ARGUMENT(  8, cfg % out_path )
        CALL GET_COMMAND_ARGUMENT(  9, sy_str )
        CALL GET_COMMAND_ARGUMENT( 10, ey_str )
        CALL GET_COMMAND_ARGUMENT( 11, sm_str )
        CALL GET_COMMAND_ARGUMENT( 12, em_str )
        CALL GET_COMMAND_ARGUMENT( 13, sd_str )
        CALL GET_COMMAND_ARGUMENT( 14, ed_str )
    ELSE
        PRINT*, " --- ERROR! 14 arguments are expected !"
        STOP
    END IF

    ! convert strings to real & integer
    READ( thv_str, '(F10.6)' ) cfg % thv
    READ( mpc_str, '(I1)' ) cfg % mpc
    READ( cwc_mod_str, '(I1)' ) cfg % cwc_mod
    READ( overlap_str, '(I1)' ) cfg % overlap
    READ( scops_str, '(I1)' ) cfg % scops
    READ( sy_str, '(I4)' ) cfg % sy
    READ( ey_str, '(I4)' ) cfg % ey
    READ( sm_str, '(I2)' ) cfg % sm
    READ( em_str, '(I2)' ) cfg % em
    READ( sd_str, '(I2)' ) cfg % sd
    READ( ed_str, '(I2)' ) cfg % ed

    ! get config settings
    CALL READ_CONFIG( cfg )

    ! create output directory if not already existing
    CALL CREATE_DIR( TRIM(cfg % out_path) )

    ! create land/sea mask from SST & era-grid
    CALL READ_AUX_DATA( TRIM(cfg % sst_file), aux )
    x = aux % nlon
    y = aux % nlat

    ! loop over year and month
    DO year = cfg % sy, cfg % ey, ystep
        DO month = cfg % sm, cfg % em, mstep

            CALL CPU_TIME( start_month )

            CALL INITIALIZE_FINAL( cfg, aux, final )
            CALL INITIALIZE_COUNTS( aux, counts )

            CALL GET_FILE_LIST(cfg, year, month, files, nfiles)

            DO ff = 1, nfiles ! loop over files per month

                CALL CONVERT_ERA_FILE( files(ff,1), ncfile )
                CALL READ_ERA_NCFILE( ncfile, input, cfg )
                CALL INIT_SZA( input, aux )

                CALL INITIALIZE_TEMPS( input, temps )
                CALL CALC_INCLOUD_CWC( input, temps )
                CALL CALC_CLD_VARS( input, aux, temps )

                CALL CPU_TIME( start )
                CALL MAIN_PROC( cfg, input, temps, final )
                CALL CPU_TIME( finish )
                PRINT '("    Elapsed Time = ",f6.3," seconds.")',& 
                    finish-start

                CALL SUMUP_VARS( temps, final, counts )
                counts % file_counter = counts % file_counter + 1

                CALL UNDEFINE_TEMPS( temps )
                CALL UNDEFINE_INPUT( input )

                IF ( (cfg % ed - cfg % sd) == 0 .AND. (ff == 4) ) EXIT

            END DO ! end of files

            IF ( ALLOCATED( files ) ) DEALLOCATE( files )

            CALL MEAN_VARS( final, counts )
            CALL WRITE_MONTHLY_MEAN( aux, cfg, final, counts )

            CALL UNDEFINE_FINAL( final )
            CALL UNDEFINE_COUNTS( counts )

            CALL CPU_TIME( finish_month )
            PRINT*, ""
            PRINT '(" ++ Elapsed time = ",F8.3," minutes for ",I4, "/", I2)',&
                (finish_month - start_month)/60., year, month

        END DO !end of month-loop
    END DO !end of year-loop

    CALL UNDEFINE_AUX( aux )

    PRINT*, ""
    PRINT*, "** cloud_simulator finished for ", &
        TRIM(cfg%start_date), " - ", TRIM(cfg%end_date)
    PRINT*, ""

!===============================================================================
END PROGRAM CLOUD_SIMULATOR
!===============================================================================
