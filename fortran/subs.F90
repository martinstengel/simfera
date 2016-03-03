      MODULE subs

        USE STRUCTS

        CONTAINS


        SUBROUTINE GET_FILE_LIST(cfg, year, month, file_list)

            IMPLICIT NONE
            REAL :: r
            TYPE(config) :: cfg
            CHARACTER(LEN=4) :: ystr
            CHARACTER(LEN=2) :: mstr
            INTEGER(KIND=sint) :: i, reason, nfiles, ilen
            INTEGER(KIND=sint) :: year, month
            CHARACTER(LEN=500) :: command, txtfile, inppath, line
            CHARACTER(LEN=500), ALLOCATABLE :: file_list(:,:)

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

        END SUBROUTINE GET_FILE_LIST


        SUBROUTINE CONVERT_ERA_FILE( efile )

            IMPLICIT NONE

            CHARACTER(LEN=900) :: command
            CHARACTER(LEN=500) :: efile
            LOGICAL            :: file_exists

            INQUIRE( FILE=TRIM(efile)//'.nc', EXIST=file_exists )

            IF (file_exists) THEN 
                WRITE (*,*) "** ERA nc-file already exists: '"//&
                            TRIM(efile)//'.nc'//"'"
            ELSE
                command = "cdo -f nc copy "//TRIM(efile)//&
                          " "//TRIM(efile)//'.nc'

                WRITE(*,'(a)') "** "//TRIM(command)
                CALL system( command )
            ENDIF
            PRINT*, ''

        END SUBROUTINE CONVERT_ERA_FILE


        SUBROUTINE CREATE_DIR( newDirPath )
        
            IMPLICIT NONE
        
            CHARACTER(LEN=*), INTENT(IN) :: newDirPath
            CHARACTER(LEN=256)           :: mkdirCmd
            LOGICAL                      :: dirExists
        
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
            PRINT*, ''

        END SUBROUTINE CREATE_DIR


      END MODULE subs
