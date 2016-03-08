!===============================================================================
! Name: cloud_simulator.F90
!===============================================================================
PROGRAM CLOUD_SIMULATOR
!=============================================================================== 

    USE COMMON_CONSTANTS
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
    CHARACTER(LEN=file_length)              :: ncfile
    CHARACTER(LEN=file_length), ALLOCATABLE :: files(:,:)

    PRINT*, ""
    PRINT*, "** cloud_simulator started"
    PRINT*, ""

    ! get config settings
    CALL READ_CONFIG(cfg)

    ! create output directory if not already existing
    CALL CREATE_DIR( TRIM(cfg%out_path) )

    ! create land/sea mask from SST & era-grid
    CALL READ_AUX_DATA( TRIM(cfg%sst_file), aux )

    ! loop over year and month
    DO year = cfg%sy, cfg%ey, ystep !year-loop 
        DO month = cfg%sm, cfg%em, mstep !month-loop

            CALL GET_FILE_LIST(cfg, year, month, files, nfiles)

            DO ff = 1, nfiles ! loop over files per month

                CALL CONVERT_ERA_FILE( files(ff,1), ncfile )
                CALL READ_ERA_NCFILE( ncfile, input )
                CALL INIT_SZA( input, aux )

                CALL CALC_INCLOUD_CWC( input, temps )
                CALL CALC_CLD_VARS( input, aux, temps )
                CALL MAIN_PROC( cfg, input, temps, final )
                stop

                CALL UNDEFINE_TEMPS( temps )
                CALL UNDEFINE_INPUT( input )

            END DO ! end of files

            IF ( ALLOCATED( files ) ) DEALLOCATE( files )

            ! make MM(L3) product and save it

            CALL UNDEFINE_FINAL( final )

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
