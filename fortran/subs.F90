!-------------------------------------------------------------------------------
! Name: subs.F90
!-------------------------------------------------------------------------------
MODULE SUBS 

    CONTAINS

    !==========================================================================
    
    SUBROUTINE PRINT_MINMAX ( what, x, y, array )
        INTEGER           :: x, y
        REAL              :: array(x,y)
        CHARACTER(LEN=20) :: what
        PRINT('(A30, 2F14.6)'), what, minval(array), maxval(array)
    END SUBROUTINE PRINT_MINMAX

    !==========================================================================

    SUBROUTINE GET_FILE_LIST ( cfg, year, month, file_list, nfiles )

        USE STRUCTS
        USE COMMON_CONSTANTS
    
        IMPLICIT NONE
    
        TYPE(config),       INTENT(IN)                       :: cfg
        INTEGER(KIND=sint), INTENT(IN)                       :: year, month
        INTEGER(KIND=sint), INTENT(OUT)                      :: nfiles
        CHARACTER(LEN=file_length), ALLOCATABLE, INTENT(OUT) :: file_list(:,:)

        ! local variables
        REAL                       :: r
        CHARACTER(LEN=4)           :: ystr
        CHARACTER(LEN=2)           :: mstr
        INTEGER(KIND=sint)         :: i, reason, ilen
        CHARACTER(LEN=file_length) :: command, txtfile, inppath, line
    
        PRINT*, "** GET_FILE_LIST"

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
        ! x-rows, 1 column
        ALLOCATE(file_list(nfiles,1))
    
        REWIND(31)
        DO i = 1, nfiles
            READ(31,"(A)") line
            ilen = LEN_TRIM(line)
            READ(TRIM(line(1:ilen)),'(A)') file_list(i,1)
        END DO
    
        CLOSE(31)

        PRINT('(I6,A16,I5,I3)'), nfiles,"ERA-I files for", year, month
    
    END SUBROUTINE GET_FILE_LIST

    !==========================================================================

    SUBROUTINE CONVERT_ERA_FILE ( ifile, ofile )
    
        USE COMMON_CONSTANTS
        IMPLICIT NONE
    
        CHARACTER(LEN=file_length), INTENT(IN)  :: ifile
        CHARACTER(LEN=file_length), INTENT(OUT) :: ofile

        !local variables 
        LOGICAL            :: file_exists
        CHARACTER(LEN=900) :: command

        PRINT*, "** CONVERT_ERA_FILE"

        ofile = TRIM(ifile)//'.nc'
        INQUIRE( FILE=TRIM(ofile), EXIST=file_exists )
    
        IF (file_exists) THEN 
            PRINT*, "   ERA ncfile already exists: '"//TRIM(ofile)//"'"
        ELSE
            command = "cdo -f nc copy "//TRIM(ifile)//" "//TRIM(ofile)
            WRITE(*,'(a)') " ** "//TRIM(command)
            CALL system( command )
        ENDIF
    
    END SUBROUTINE CONVERT_ERA_FILE

    !==========================================================================

    SUBROUTINE CREATE_DIR ( newDirPath )
    
        USE COMMON_CONSTANTS
    
        IMPLICIT NONE
    
        CHARACTER(LEN=path_length), INTENT(IN) :: newDirPath

        !local variables
        LOGICAL            :: dirExists
        CHARACTER(LEN=256) :: mkdirCmd
    
        PRINT*, '** CREATE_DIR'

        ! Check if the directory exists first
        ! Works with ifort, but not gfortran
        !INQUIRE( directory=newDirPath, EXIST=dirExists ) 
        ! Works with gfortran, but not ifort
        INQUIRE( FILE=TRIM(newDirPath)//'/.', EXIST=dirExists )  
    
        IF (dirExists) THEN 
            WRITE (*,*) "   Directory already exists: '"//&
                        TRIM(newDirPath)//"'"
        ELSE
            mkdirCmd = 'mkdir -p '//TRIM(newDirPath)
            WRITE(*,'(a)') "   Creating new directory: '"//&
                           TRIM(mkdirCmd)//"'"
            CALL system( mkdirCmd )
        ENDIF
    
    END SUBROUTINE CREATE_DIR

    
    !==========================================================================

    SUBROUTINE CALC_INCLOUD_CWC ( inp, tmp )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        TYPE(era_input),  INTENT(IN)    :: inp
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp

        !local variable
        INTEGER(KIND=sint) :: z

        PRINT*, "** CALC_INCLOUD_CWC"

        DO z=inp%zdim, 1, -1

            ! incloud liquid water content
            WHERE( inp%cc(:,:,z) .GT. 0. .AND. inp%lwc(:,:,z) .GT. 0 )
                tmp%lwc_inc(:,:,z) = inp%lwc(:,:,z) / inp%cc(:,:,z)
            ELSEWHERE
                tmp%lwc_inc(:,:,z) = 0.
            END WHERE

            ! incloud ice water content
            WHERE( inp%cc(:,:,z) .GT. 0. .AND. inp%iwc(:,:,z) .GT. 0 )
                tmp%iwc_inc(:,:,z) = inp%iwc(:,:,z) / inp%cc(:,:,z)
            ELSEWHERE
                tmp%iwc_inc(:,:,z) = 0.
            END WHERE

        END DO

    END SUBROUTINE

    !==========================================================================

    SUBROUTINE CALC_CLD_VARS ( inp, aux, tmp )

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE FUNCS, only: GET_LIQ_CER, GET_ICE_CER

        IMPLICIT NONE

        TYPE(era_input),  INTENT(IN)    :: inp
        TYPE(era_aux),    INTENT(IN)    :: aux
        TYPE(tmp_arrays), INTENT(INOUT) :: tmp

        ! local variables
        INTEGER(KIND=sint)                             :: z
        REAL(KIND=sreal)                               :: pressure
        REAL(KIND=sreal), DIMENSION(inp%xdim,inp%ydim) :: lwc_z, iwc_z
        REAL(KIND=sreal), DIMENSION(inp%xdim,inp%ydim) :: temperature

        PRINT*, "** CALC_CLD_VARS"

        DO z=inp%zdim-1, 1, -1

            lwc_z = tmp % lwc_inc(:,:,z)*0.5 + tmp % lwc_inc(:,:,z+1)*0.5
            iwc_z = tmp % iwc_inc(:,:,z)*0.5 + tmp % iwc_inc(:,:,z+1)*0.5

            temperature = inp % temp(:,:,z)*0.5 + inp % temp(:,:,z+1)*0.5 ![K]
            pressure = inp % plevel(z)*0.5 + inp % plevel(z+1)*0.5 ![Pa]

            tmp % lwp_lay(:,:,z) = lwc_z * inp % dpres(z) / 9.81 ![kg/m2]
            tmp % iwp_lay(:,:,z) = iwc_z * inp % dpres(z) / 9.81 ![kg/m2]

            tmp % lcer_lay(:,:,z) = GET_LIQ_CER( temperature, lwc_z, pressure,&
                                                 aux%lsm2d, inp%xdim, inp%ydim  )
            tmp % icer_lay(:,:,z) = GET_ICE_CER( temperature, iwc_z, pressure, &
                                                 inp%xdim, inp%ydim  )

            ! COT computation: method of Han et al. (1994)
            !   CWP = (4 * COT * R_eff * rho) / (3 * Q_ext)
            !   COT = (3 * CWP * Q_ext) / (4 * R_eff * rho)

            tmp % lcot_lay(:,:,z) = (3.0*tmp % lwp_lay(:,:,z)*qext_water) / &
                                    (4.0*tmp % lcer_lay(:,:,z)*1.0E-6*rho_water)
            tmp % icot_lay(:,:,z) = (3.0*tmp % iwp_lay(:,:,z)*qext_ice) / &
                                    (4.0*tmp % icer_lay(:,:,z)*1.0E-6*rho_ice)

            WHERE ( tmp % lwp_lay(:,:,z) == 0.0 )
                tmp % lcot_lay(:,:,z) = 0.0
                tmp % lcer_lay(:,:,z) = 0.0
            END WHERE
            WHERE ( tmp % iwp_lay(:,:,z) == 0.0 )
                tmp % icot_lay(:,:,z) = 0.0
                tmp % icer_lay(:,:,z) = 0.0
            END WHERE

        END DO

    END SUBROUTINE

    !==========================================================================

    SUBROUTINE INIT_SZA ( inp, aux )

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE FUNCS, only: DAY_OF_YEAR, GET_SZA

        IMPLICIT NONE

        TYPE(era_aux),   INTENT(IN)    :: aux
        TYPE(era_input), INTENT(INOUT) :: inp

        ! local variables
        INTEGER(KIND=sint), DIMENSION(inp%xdim,inp%ydim) :: doy
        INTEGER(KIND=sint), DIMENSION(inp%xdim,inp%ydim) :: hour
        INTEGER(KIND=sint), DIMENSION(inp%xdim,inp%ydim) :: minute

        PRINT*, "** INIT_SZA"

        ! check if SST grid is the same of input file
        IF (inp % xdim .NE. aux % nlon) STOP
        IF (inp % ydim .NE. aux % nlat) STOP

        doy = DAY_OF_YEAR( inp % year, inp % month, inp % day )
        hour = inp % hour
        minute = 0
        inp % doy = doy(1,1)
        inp % sza2d = GET_SZA(doy, hour, minute, aux % lon2d, &
                              aux % lat2d, aux % nlon, aux % nlat)

    END SUBROUTINE INIT_SZA

    !==========================================================================

    SUBROUTINE READ_CONFIG ( cfile, cfg )
    
        USE COMMON_CONSTANTS
        USE STRUCTS
        USE FUNCS, only: NUMBER_OF_DAYS
    
        IMPLICIT NONE
    
        CHARACTER(LEN=path_length), INTENT(IN)    :: cfile
        TYPE(config),               INTENT(INOUT) :: cfg

        ! local variables
        INTEGER(KIND=sint) :: io, idx, lun, ilen
        CHARACTER(LEN=4)   :: year
        CHARACTER(LEN=2)   :: month, day
        CHARACTER(LEN=500) :: line, what
    
        PRINT*, "** READ_CONFIG: ", TRIM(cfile)

        OPEN(NEWUNIT=lun, FILE=TRIM(cfile), STATUS="old")
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
                        PRINT*, "   NOT defined in config.file"
                        STOP
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
        cfg % hist_phase = (/0.0, 1.0/)
    
        ! 2d histogram
        cfg % hist_cot_2d_axis=(/0.0, 0.3, 0.6, 1.3, 2.2, 3.6, 5.8, & 
                                 9.4, 15.0, 23.0, 41.0, 60.0, 80.0, 100./)
        cfg % hist_cot_2d_bin=cfg % hist_cot_2d_axis(1:n_hist_cot-1)*0.5 + &
                              cfg % hist_cot_2d_axis(2:n_hist_cot)*0.5
    
        cfg % hist_ctp_2d_axis=(/1.0, 90.0, 180.0, 245.0, 310.0, 375.0, &
                                 440.0, 500.0, 560.0, 620.0, 680.0, &
                                 740.0, 800.0, 875.0, 950.0, 1100./)
        cfg % hist_ctp_2d_bin=cfg % hist_ctp_2d_axis(1:n_hist_ctp-1)*0.5 + &
                              cfg % hist_ctp_2d_axis(2:n_hist_ctp)*0.5
    
        ! 1d histogram
        cfg % hist_cot_1d_axis=(/0.0, 0.3, 0.6, 1.3, 2.2, 3.6, 5.8, 9.4, &
                                 15.0, 23.0, 41.0, 60.0, 80.0, 99.99, 1000./) 
        cfg % hist_cot_1d_bin=cfg % hist_cot_1d_axis(1:n_cot_bins)*0.5 + &
                              cfg % hist_cot_1d_axis(2:n_cot_bins+1)*0.5
    
        cfg % hist_ctp_1d_axis=(/ 1.0, 90.0, 180.0, 245.0, 310.0, 375.0, &
                                  440.0, 500.0, 560.0, 620.0, 680.0, 740.0, 800.0, &
                                  875.0, 950.0, 1100.0 /)
        cfg % hist_ctp_1d_bin=cfg % hist_ctp_1d_axis(1:n_ctp_bins)*0.5 + &
                              cfg % hist_ctp_1d_axis(2:n_ctp_bins+1)*0.5
    
        cfg % hist_ctt_1d_axis=(/ 200.0, 210.0, 220.0, 230.0, 235.0, &
                                  240.0, 245.0, 250.0, 255.0, 260.0, 265.0, &
                                  270.0, 280.0, 290.0, 300.0, 310.0, 350.0 /)
        cfg % hist_ctt_1d_bin=cfg % hist_ctt_1d_axis(1:n_ctt_bins)*0.5 + & 
                              cfg % hist_ctt_1d_axis(2:n_ctt_bins+1)*0.5
    
        cfg % hist_cwp_1d_axis=(/ 0.0, 5.0, 10.0, 20.0, 35.0, 50.0, &
                                  75.0, 100.0, 150.0, 200.0, 300.0, 500.0, &
                                  1000.0, 2000.0, 100000.0 /)
        cfg % hist_cwp_1d_bin=cfg % hist_cwp_1d_axis(1:n_cwp_bins)*0.5 + &
                              cfg % hist_cwp_1d_axis(2:n_cwp_bins+1)*0.5
    
        cfg % hist_cer_1d_axis=(/ 0.0, 3.0, 6.0, 9.0, 12.0, 15.0, &
                                  20.0, 25.0, 30.0, 40.0, 60.0, 80.0 /)
        cfg % hist_cer_1d_bin=cfg % hist_cer_1d_axis(1:n_cer_bins)*0.5 + &
                              cfg % hist_cer_1d_axis(2:n_cer_bins+1)*0.5
    
        PRINT('(A13, E8.2)'), "COT-THV: ", cfg%thv
        PRINT('(A13, I1)'), "MPC: ", cfg%mpc
        PRINT('(A13, I1)'), "SCOPS: ", cfg%scops
        PRINT('(A13, A8)'), "START: ", cfg%start_date
        PRINT('(A13, A8)'), "STOP: ", cfg%end_date
        PRINT('(A13, A)'), "REP_PWD: ", TRIM(cfg%rep_path)
        PRINT('(A13, A)'), "INP_PWD: ", TRIM(cfg%inp_path)
        PRINT('(A13, A)'), "OUT_PWD: ", TRIM(cfg%out_path)
        PRINT('(A13, A)'), "SST_FILE: ", TRIM(cfg%sst_file)

    END SUBROUTINE READ_CONFIG

    !==========================================================================

    SUBROUTINE ADDTO ( flag, i, j, input, output, counts )

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER,            INTENT(IN)      :: flag
        INTEGER(KIND=sint), INTENT(IN)      :: i, j
        REAL(KIND=sreal),   INTENT(IN)      :: input(i,j)
        REAL(KIND=sreal),   INTENT(INOUT)   :: output(i,j)
        INTEGER(KIND=lint), INTENT(INOUT)   :: counts(i,j)

        IF ( flag == 1 ) THEN ! input GE 0.0

            WHERE ( input >= 0.0 )
                output = output + input
                counts = counts + 1
            END WHERE

        ELSE ! input GT 0.0

            WHERE ( input > 0.0 )
                output = output + input
                counts = counts + 1
            END WHERE

        END IF

    END SUBROUTINE ADDTO

    !==========================================================================

    SUBROUTINE SUMUP_VARS ( tmp, fin, cnt )

        USE STRUCTS

        IMPLICIT NONE

        TYPE(tmp_arrays), INTENT(IN)    :: tmp
        TYPE(l3_vars),    INTENT(INOUT) :: fin
        TYPE(npoints),    INTENT(INOUT) :: cnt

        ! local variables
        INTEGER(KIND=sint) :: x, y

        PRINT*, "** SUMUP_VARS"

        x = SIZE( fin % cfc, 1 )
        y = SIZE( fin % cfc, 2 )

        WHERE ( tmp % ctp > 10.0 .AND. tmp % cth > 0.0 .AND. tmp % cph >= 0.0 )
            fin % ctp = fin % ctp + tmp % ctp
            fin % cth = fin % cth + tmp % cth
            fin % ctt = fin % ctt + tmp % ctt
            fin % cph = fin % cph + tmp % cph
            cnt % ctp = cnt % ctp + 1
        END WHERE

        CALL ADDTO( 1, x, y, tmp % cfc,        fin % cfc,        cnt % cfc )
        CALL ADDTO( 1, x, y, tmp % cph_day,    fin % cph_day,    cnt % cph_day )
        CALL ADDTO( 1, x, y, tmp % cwp_allsky, fin % cwp_allsky, cnt % cwp_allsky )
        CALL ADDTO( 1, x, y, tmp % lwp_allsky, fin % lwp_allsky, cnt % lwp_allsky )
        CALL ADDTO( 1, x, y, tmp % iwp_allsky, fin % iwp_allsky, cnt % iwp_allsky )

        CALL ADDTO( 2, x, y, tmp % cer,     fin % cer,     cnt % cer )
        CALL ADDTO( 2, x, y, tmp % cer_liq, fin % cer_liq, cnt % cer_liq )
        CALL ADDTO( 2, x, y, tmp % cer_ice, fin % cer_ice, cnt % cer_ice )
        CALL ADDTO( 2, x, y, tmp % cot,     fin % cot,     cnt % cot )
        CALL ADDTO( 2, x, y, tmp % cot_liq, fin % cot_liq, cnt % cot_liq )
        CALL ADDTO( 2, x, y, tmp % cot_ice, fin % cot_ice, cnt % cot_ice )
        CALL ADDTO( 2, x, y, tmp % cwp,     fin % cwp,     cnt % cwp )
        CALL ADDTO( 2, x, y, tmp % lwp,     fin % lwp,     cnt % lwp )
        CALL ADDTO( 2, x, y, tmp % iwp,     fin % iwp,     cnt % iwp )

    END SUBROUTINE SUMUP_VARS

    !==========================================================================

    SUBROUTINE AVG ( scale_factor, i, j, arr, num )

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        REAL(KIND=sreal),   INTENT(IN)      :: scale_factor
        INTEGER(KIND=sint), INTENT(IN)      :: i, j
        REAL(KIND=sreal),   INTENT(INOUT)   :: arr(i,j)
        INTEGER(KIND=lint), INTENT(INOUT)   :: num(i,j)

        WHERE ( num > 0 ) 
            arr = ( arr / num ) * scale_factor
        ELSEWHERE
            arr = sreal_fill_value
        END WHERE

    END SUBROUTINE AVG

    !==========================================================================
    
    SUBROUTINE MEAN_VARS ( fin, cnt )

        USE COMMON_CONSTANTS
        USE STRUCTS

        IMPLICIT NONE

        TYPE(l3_vars),    INTENT(INOUT) :: fin
        TYPE(npoints),    INTENT(INOUT) :: cnt

        ! local variables
        INTEGER(KIND=sint) :: x, y
        REAL(KIND=sreal)   :: sf !scale_factor

        PRINT*, "** MEAN_VARS"

        x = SIZE( fin % cfc, 1 )
        y = SIZE( fin % cfc, 2 )

        WHERE ( cnt % ctp > 0 ) 
            fin % ctp =   fin % ctp / cnt % ctp
            fin % ctt =   fin % ctt / cnt % ctp
            fin % cph =   fin % cph / cnt % ctp
            fin % cth = ( fin % cth / cnt % ctp ) / 1000.0
        ELSEWHERE
            fin % ctp = sreal_fill_value
            fin % ctt = sreal_fill_value
            fin % cph = sreal_fill_value
            fin % cth = sreal_fill_value
        END WHERE

        sf = 1.0
        CALL AVG ( sf, x, y, fin % cfc,     cnt % cfc )
        CALL AVG ( sf, x, y, fin % cph_day, cnt % cph_day )
        CALL AVG ( sf, x, y, fin % cot,     cnt % cot )
        CALL AVG ( sf, x, y, fin % cot_liq, cnt % cot_liq )
        CALL AVG ( sf, x, y, fin % cot_ice, cnt % cot_ice )
        CALL AVG ( sf, x, y, fin % cer,     cnt % cer )
        CALL AVG ( sf, x, y, fin % cer_liq, cnt % cer_liq )
        CALL AVG ( sf, x, y, fin % cer_ice, cnt % cer_ice )

        sf = 1000.0
        CALL AVG ( sf, x, y, fin % cwp, cnt % cwp )
        CALL AVG ( sf, x, y, fin % lwp, cnt % lwp )
        CALL AVG ( sf, x, y, fin % iwp, cnt % iwp )
        CALL AVG ( sf, x, y, fin % cwp_allsky, cnt % cwp_allsky )
        CALL AVG ( sf, x, y, fin % lwp_allsky, cnt % lwp_allsky )
        CALL AVG ( sf, x, y, fin % iwp_allsky, cnt % iwp_allsky )

    END SUBROUTINE MEAN_VARS

    !==========================================================================

END MODULE SUBS
