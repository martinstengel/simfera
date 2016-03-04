!===============================================================================
! Name: cloud_simulator.F90
!===============================================================================
PROGRAM CLOUD_SIMULATOR
!=============================================================================== 

    USE COMMON_CONSTANTS
    USE STRUCTS
    USE SUBS

    IMPLICIT NONE

    TYPE(config)                    :: cfg
    INTEGER(KIND=sint)              :: year, ystep=1
    INTEGER(KIND=sint)              :: month, mstep=1
    INTEGER(KIND=sint)              :: day, dstep=1
    INTEGER(KIND=sint)              :: ff, nfiles
    CHARACTER(LEN=500), ALLOCATABLE :: files(:,:)


    ! get config settings
    CALL READ_CONFIG(cfg)

    PRINT*, ''
    PRINT*, "** cloud_simulator started with: "
    PRINT('(A15, E8.2)'), "COT-THV: ", cfg%thv
    PRINT('(A15, I1)'), "MPC: ", cfg%mpc
    PRINT('(A15, I1)'), "SCOPS: ", cfg%scops
    PRINT('(A15, A8)'), "START: ", cfg%start_date
    PRINT('(A15, A8)'), "STOP: ", cfg%end_date
    PRINT('(A15, A)'), "REP_PWD: ", TRIM(cfg%rep_path)
    PRINT('(A15, A)'), "INP_PWD: ", TRIM(cfg%inp_path)
    PRINT('(A15, A)'), "OUT_PWD: ", TRIM(cfg%out_path)
    PRINT('(A15, A)'), "SST_FILE: ", TRIM(cfg%sst_file)
    PRINT*, ''

    ! create output directory
    CALL CREATE_DIR( TRIM(cfg%out_path) )

    ! loop over year and month
    DO year = cfg%sy, cfg%ey, ystep !year-loop 
        DO month = cfg%sm, cfg%em, mstep !month-loop

            CALL GET_FILE_LIST(cfg, year, month, files)
            nfiles = SIZE(files, DIM=1)
            PRINT*, "** ",nfiles," ERA-I files for ", year, month

            ! loop over files in month
            DO ff = 1, nfiles

                ! convert to nc-file if not existing
                CALL CONVERT_ERA_FILE(TRIM(files(ff,1)))

            END DO ! end of files


            ! make MM(L3) product and save it

        END DO !end of month-loop
    END DO !end of year-loop

    PRINT*, "** cloud_simulator finished for ", &
        TRIM(cfg%start_date), " - ", TRIM(cfg%end_date)
    PRINT*, ""

!===============================================================================
END PROGRAM CLOUD_SIMULATOR
!===============================================================================
