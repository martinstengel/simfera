      ! read config file for cloud simulator
      SUBROUTINE READ_CONFIG(cfg)

        USE COMMON_CONSTANTS
        USE STRUCTS
        USE FUNCS, only: NUMBER_OF_DAYS

        IMPLICIT NONE

        INTEGER :: io, idx, lun, ilen
        CHARACTER(LEN=4) :: year
        CHARACTER(LEN=2) :: month, day
        CHARACTER*200 :: line, what
        TYPE(config), INTENT(INOUT) :: cfg

        OPEN(NEWUNIT=lun, FILE="config.file", STATUS="old")
        DO
            READ(lun,"(a)",IOSTAT=io) line
            IF (io>0) THEN
                WRITE(*,*) "Check input. Something was wrong."
                EXIT
            ELSE IF (io<0) THEN
                !WRITE(*,*) "END OF FILE"
                EXIT
            ELSE
                IF ((line(1:1) == '#') .OR. &
                    (line(1:1) == ';') .OR. &
                    (LEN_TRIM(line) == 0) ) THEN
                    CYCLE
                ELSE
                    idx = SCAN(line,"=")
                    what = TRIM(line(:idx-1))
                    ilen = LEN_TRIM(line)
                    !PRINT*, TRIM(what), " : ", TRIM(line(idx+1:))

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

      END SUBROUTINE READ_CONFIG
