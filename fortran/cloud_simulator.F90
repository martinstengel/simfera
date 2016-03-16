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

    INTEGER(KIND=sint)  :: year, ystep=1, month, mstep=1
    INTEGER(KIND=sint)  :: ff, nfiles
    TYPE(config)        :: cfg
    TYPE(era_input)     :: input
    TYPE(era_aux)       :: aux
    TYPE(tmp_arrays)    :: temps
    TYPE(l3_vars)       :: final
    TYPE(npoints)       :: counts
    CHARACTER(LEN=file_length)              :: ncfile
    CHARACTER(LEN=file_length), ALLOCATABLE :: files(:,:)

    ! local variables
    INTEGER :: x, y
    REAL    :: start, finish, start_month, finish_month

    PRINT*, ""
    PRINT*, "** cloud_simulator started"
    PRINT*, ""

    ! get config settings
    CALL READ_CONFIG(cfg)

    ! create output directory if not already existing
    CALL CREATE_DIR( TRIM(cfg % out_path) )

    ! create land/sea mask from SST & era-grid
    CALL READ_AUX_DATA( TRIM(cfg % sst_file), aux )
    x = aux % nlon
    y = aux % nlat

    ! loop over year and month
    DO year = cfg%sy, cfg%ey, ystep
        DO month = cfg%sm, cfg%em, mstep

            CALL CPU_TIME( start_month )

            CALL INITIALIZE_FINAL( cfg, aux, final )
            CALL INITIALIZE_COUNTS( aux, counts )

            CALL GET_FILE_LIST(cfg, year, month, files, nfiles)

            DO ff = 1, nfiles ! loop over files per month

                CALL CONVERT_ERA_FILE( files(ff,1), ncfile )
                CALL READ_ERA_NCFILE( ncfile, input )
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

            END DO ! end of files

            IF ( ALLOCATED( files ) ) DEALLOCATE( files )

            CALL MEAN_VARS( final, counts )
            CALL WRITE_MONTHLY_MEAN( aux, cfg, final, counts )

            CALL PRINT_MINMAX( "cfc",        x, y, final % cfc )
            CALL PRINT_MINMAX( "cph",        x, y, final % cph )
            CALL PRINT_MINMAX( "cph_day",    x, y, final % cph_day )
            CALL PRINT_MINMAX( "ctp",        x, y, final % ctp )
            CALL PRINT_MINMAX( "cth",        x, y, final % cth )
            CALL PRINT_MINMAX( "ctt",        x, y, final % ctt )
            CALL PRINT_MINMAX( "cwp",        x, y, final % cwp )
            CALL PRINT_MINMAX( "lwp",        x, y, final % lwp )
            CALL PRINT_MINMAX( "iwp",        x, y, final % iwp )
            CALL PRINT_MINMAX( "cwp_allsky", x, y, final % cwp_allsky )
            CALL PRINT_MINMAX( "lwp_allsky", x, y, final % lwp_allsky )
            CALL PRINT_MINMAX( "iwp_allsky", x, y, final % iwp_allsky )
            CALL PRINT_MINMAX( "cot",        x, y, final % cot )
            CALL PRINT_MINMAX( "cot_liq",    x, y, final % cot_liq )
            CALL PRINT_MINMAX( "cot_ice",    x, y, final % cot_ice )
            CALL PRINT_MINMAX( "cer",        x, y, final % cer )
            CALL PRINT_MINMAX( "cer_liq",    x, y, final % cer_liq )
            CALL PRINT_MINMAX( "cer_ice",    x, y, final % cer_ice )

            CALL UNDEFINE_FINAL( final )
            CALL UNDEFINE_COUNTS( counts )

            CALL CPU_TIME( finish_month )
            PRINT*, ""
            PRINT '(" ++ Elapsed time = ",F6.3," seconds for ",I4, "/", I2)',&
                finish_month - start_month, year, month

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
