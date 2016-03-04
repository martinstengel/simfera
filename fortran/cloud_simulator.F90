!===============================================================================
! Name: cloud_simulator.F90
!===============================================================================
PROGRAM CLOUD_SIMULATOR
!=============================================================================== 

    USE COMMON_CONSTANTS
    USE STRUCTS
    USE SUBS

    IMPLICIT NONE

    INTEGER(KIND=sint)  :: year, ystep=1
    INTEGER(KIND=sint)  :: month, mstep=1
    INTEGER(KIND=sint)  :: day, dstep=1
    INTEGER(KIND=sint)  :: ff, nfiles

    TYPE(config)      :: cfg
    TYPE(era_input)   :: input
    TYPE(era_sst_lsm) :: aux

    CHARACTER(LEN=file_length)              :: ncfile
    CHARACTER(LEN=file_length), ALLOCATABLE :: files(:,:)

    PRINT*, ""
    PRINT*, "** cloud_simulator started"

    ! get config settings
    CALL READ_CONFIG(cfg)

    ! create output directory
    CALL CREATE_DIR( TRIM(cfg%out_path) )

    ! read sea surface temperature and get land/sea mask
    CALL READ_AUX_DATA( TRIM(cfg%sst_file), aux )


    ! loop over year and month
    DO year = cfg%sy, cfg%ey, ystep !year-loop 
        DO month = cfg%sm, cfg%em, mstep !month-loop

            CALL GET_FILE_LIST(cfg, year, month, files)
            nfiles = SIZE(files, DIM=1)

            DO ff = 1, nfiles ! loop over files per month

                CALL CONVERT_ERA_FILE( files(ff,1), ncfile )

                CALL READ_ERA_NCFILE( ncfile, input )

                CALL DEALLOCATE_INPUT( input )

            END DO ! end of files


            ! make MM(L3) product and save it

        END DO !end of month-loop
    END DO !end of year-loop

    CALL DEALLOCATE_AUX( aux )

    PRINT*, ""
    PRINT*, "** cloud_simulator finished for ", &
        TRIM(cfg%start_date), " - ", TRIM(cfg%end_date)
    PRINT*, ""

!===============================================================================
END PROGRAM CLOUD_SIMULATOR
!===============================================================================
