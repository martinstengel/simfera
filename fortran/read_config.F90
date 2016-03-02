      ! read config file for cloud simulator
      SUBROUTINE read_config (thv, mpc, scops, rep_path, inp_path, & 
                          out_path, sst_file, sy, ey, sm, em, sd, ed)

      IMPLICIT NONE

      INTEGER :: mpc, scops, lun
      INTEGER :: sy, ey, sm, em, sd, ed
      REAL(kind=4) :: thv
      CHARACTER*1024 :: rep_path, inp_path, out_path, sst_file

      !OPEN(NEWUNIT=lun, FILE="config.file", STATUS="old")
      !CLOSE(lun)

      WRITE(*,*) thv, mpc, scops
      WRITE(*,*) sy, ey, sm, em, sd, ed
      WRITE(*,*) TRIM(rep_path)
      WRITE(*,*) TRIM(inp_path)
      WRITE(*,*) TRIM(out_path)
      WRITE(*,*) TRIM(sst_file)

      END SUBROUTINE read_config
