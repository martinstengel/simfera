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

    PRINT*, ""
    PRINT*, "** cloud_simulator started"
    PRINT*, ""

    ! get config settings
    CALL READ_CONFIG(cfg)

    ! create output directory if not already existing
    CALL CREATE_DIR( TRIM(cfg % out_path) )

    ! create land/sea mask from SST & era-grid
    CALL READ_AUX_DATA( TRIM(cfg % sst_file), aux )

    ! loop over year and month
    DO year = cfg%sy, cfg%ey, ystep
        DO month = cfg%sm, cfg%em, mstep

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
                CALL MAIN_PROC( cfg, input, temps, final )

                CALL SUMUP_VARS( temps, final, counts )
                counts % raw = counts % raw + 1 !file count

                CALL UNDEFINE_TEMPS( temps )
                CALL UNDEFINE_INPUT( input )

            END DO ! end of files

            IF ( ALLOCATED( files ) ) DEALLOCATE( files )

            CALL MEAN_VARS( final, counts )
            stop

            ! CALL WRITE_MONTHLY_MEAN( cfg, final, counts )
            CALL UNDEFINE_FINAL( final )
            CALL UNDEFINE_COUNTS( counts )

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
