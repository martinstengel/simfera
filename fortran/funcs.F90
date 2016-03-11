!-------------------------------------------------------------------------------
! Name: funcs.F90
!-------------------------------------------------------------------------------
MODULE FUNCS 

    CONTAINS

    !==========================================================================

    FUNCTION GET_MEAN( n, vector, phase, flag ) RESULT( mean )

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=sint)             :: i, n, cnt, flag
        REAL(KIND=sreal), DIMENSION(n) :: vector, phase
        REAL(KIND=sreal)               :: mean, isum

        cnt = 0
        isum = 0.0

        DO i = 1, n
            IF ( vector(i) >= 0.0 .AND. phase(i) /= sreal_fill_value ) THEN
                cnt = cnt + 1
                isum = isum + vector(i)
            END IF
        END DO

        IF ( cnt > 0 ) THEN
            IF ( flag == normal ) mean = isum / cnt
            IF ( flag == allsky ) mean = isum / n
        ELSE
            mean = sreal_fill_value
        END IF

    END FUNCTION GET_MEAN

    !==========================================================================

    FUNCTION RANDOM_NUMBER_DUPLICATES( n, vector ) RESULT( ret )

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=sint)             :: n, i, j, cnt
        REAL(KIND=sreal), DIMENSION(n) :: vector
        LOGICAL                        :: ret

        ret = .FALSE.

        DO i=1, n
            cnt = 0
            DO j=1, n
                IF ( vector(i) == vector(j) ) cnt = cnt + 1
                IF ( cnt > 1 ) ret = .TRUE.
            END DO
        END DO

    END FUNCTION RANDOM_NUMBER_DUPLICATES

    !==========================================================================

    FUNCTION GET_DENSITY_OF_AIR( temp, pres, x, y ) RESULT( rho )

        ! https://en.wikipedia.org/wiki/Density_of_air
        ! r_specific = specific gas constant for dry air (J/(kg*K))
        ! The specific gas constant for dry air is 287.058 J/(kg·K) in SI units
        ! 1 J = 1 N m = 1 kg m^2 s^-2
        ! 1 Pa = 1 N m^-2 = 1 kg m^-1 s^-2

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=lint)               :: x, y   !nlon, nlat
        REAL(KIND=sreal)                 :: pres   !absolute pressure (Pa)
        REAL(KIND=sreal), DIMENSION(x,y) :: temp   !absolute temperature (K)
        REAL(KIND=sreal), DIMENSION(x,y) :: rho    !air density (kg/m^3)

        rho = pres / ( r_specific * temp )

    END FUNCTION GET_DENSITY_OF_AIR

    !==========================================================================

    FUNCTION GET_ICE_CER( pt, iwc, pres, x, y  ) RESULT( zradip )

        ! Sun, 2001 (corrected from Sun & Rikus, 1999)

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=lint)               :: x, y   !nlon, nlat
        REAL(KIND=sreal)                 :: pres   !air pressure
        REAL(KIND=sreal), DIMENSION(x,y) :: pt     !air temperature
        REAL(KIND=sreal), DIMENSION(x,y) :: rho    !air density
        REAL(KIND=sreal), DIMENSION(x,y) :: iwc    !ice water content [kg/kg]
        REAL(KIND=sreal), DIMENSION(x,y) :: ziwc   !ice water content [g/m3]
        REAL(KIND=sreal), DIMENSION(x,y) :: zradip !ice eff. radius [mu]
        REAL(KIND=sreal), DIMENSION(x,y) :: ztempc, ztcels, zfsr
        REAL(KIND=sreal), DIMENSION(x,y) :: zaiwc, zbiwc, zdesr

        rho  = GET_DENSITY_OF_AIR( pt, pres, x, y )
        ziwc = iwc * rho * 1000.

        ztempc = pt - 83.15
        ztcels = pt - rtt
        zfsr   = 1.2351 + 0.0105 * ztcels
        zaiwc  = 45.8966 * ( ziwc**0.2214 )
        zbiwc  =  0.7957 * ( ziwc**0.2535 )
        zdesr  = zfsr * ( zaiwc + zbiwc * ztempc )
        zdesr  = MIN( MAX( zdesr, 30.0 ), 155.0 )
        zradip = zrefde * zdesr

        !PRINT('(A24)'), " === GET_ICE_CER === "
        !PRINT('(A20,2E14.5)'), "RHO : ", minval(rho), maxval(rho)
        !PRINT('(A20,2E14.5)'), "IWC : ", minval(iwc), maxval(iwc)
        !PRINT('(A20,2E14.5)'), "ZIWC : ", minval(ziwc), maxval(ziwc)
        !PRINT('(A20,2F14.5)'), "ZRADLP : ", minval(zradip), maxval(zradip)

    END FUNCTION GET_ICE_CER

    !==========================================================================

    FUNCTION GET_LIQ_CER( temp, lwc, pres, lsm, x, y  ) RESULT( zradlp )

        ! The liq. water eff. radius in ERA-Interim follows Martin et al. 1994

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=lint)                 :: x, y   !nlon, nlat
        REAL(KIND=sreal)                   :: pres   !air pressure
        INTEGER(KIND=sint), DIMENSION(x,y) :: lsm    !land/sea mask
        REAL(KIND=sreal),   DIMENSION(x,y) :: temp   !air temperature
        REAL(KIND=sreal),   DIMENSION(x,y) :: rho    !air density
        REAL(KIND=sreal),   DIMENSION(x,y) :: lwc    !liquid water content [kg/kg]
        REAL(KIND=sreal),   DIMENSION(x,y) :: zlwc   !liquid water content [g/m3]
        REAL(KIND=sreal),   DIMENSION(x,y) :: zradlp !liq. eff. radius [mu]
        REAL(KIND=sreal),   DIMENSION(x,y) :: znum, zden, ztemp, zntot, zd

        rho  = GET_DENSITY_OF_AIR( temp, pres, x, y )
        zlwc = lwc * rho * 1000.

        WHERE ( lsm == sea  ) 
               zd = zd_sea
            zntot = zntot_sea
        END WHERE

        WHERE ( lsm == land  ) 
               zd = zd_land
            zntot = zntot_land
        END WHERE

        znum  = 3.0 * zlwc * ( 1.0 + 3.0 * zd * zd )**2
        zden  = 4.0 * pi * zntot * ( 1.0 + zd * zd )**3
        ztemp = 1.0 / zden

        WHERE ( (znum * ztemp) > 1.E-12 )
            zradlp = 100. * EXP( 0.333 * LOG( znum * ztemp ) )
            zradlp = MIN( MAX( zradlp, 4.0 ), 16.0 )
        ELSEWHERE
            zradlp = 4.0
        END WHERE

        !PRINT('(A24)'), " === GET_LIQ_CER === "
        !PRINT('(A20,2E14.5)'), "RHO : ", minval(rho), maxval(rho)
        !PRINT('(A20,2E14.5)'), "LWC : ", minval(lwc), maxval(lwc)
        !PRINT('(A20,2E14.5)'), "ZLWC : ", minval(zlwc), maxval(zlwc)
        !PRINT('(A20,2F14.5)'), "ZRADLP : ", minval(zradlp), maxval(zradlp)

    END FUNCTION GET_LIQ_CER

    !==========================================================================

    FUNCTION DAY_OF_YEAR( year, month, day ) RESULT(doy)

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=sint) :: month, day, year, i
        INTEGER(KIND=sint) :: doy, last_day
    
        doy = 0
        DO i = 1, month
            IF ( i < month ) THEN
                last_day = NUMBER_OF_DAYS( i, year )
            ELSE
                last_day = day
            END IF
            doy = doy + last_day
        END DO

    END FUNCTION DAY_OF_YEAR

    !==========================================================================

    FUNCTION GET_SZA(doy, hour, minute, lon, lat, xdim, ydim) RESULT(sza)

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=lint)                       :: xdim, ydim
        INTEGER(KIND=sint), DIMENSION(xdim,ydim) :: doy, hour, minute
        REAL(KIND=sreal),   DIMENSION(xdim,ydim) :: lon, lat, sza
        REAL(KIND=sreal),   DIMENSION(xdim,ydim) :: x, delta, z, cost, h

        ! source IDL code from Timo: ../subtools/get_sza.pro
        x     = 2.*pi/365.25*doy - 2.72*2.*pi/360. 
        delta = asin(0.39776*sin(x-77.51*2.*pi/360.+1.92*2.*pi/360.*sin(x))) 
        z     = (-7.66*sin(x)-9.87*sin(2.*x+24.99*2.*pi/360.+3.83*2.*pi/360.*sin(x)))*60. 
        h     = (2.*pi/86400.)*(hour*3600.+minute*60. -43200. + lon*4.*60. + z) 
        cost  = sin(2.*pi*lat/360.)*sin(delta) + cos(2.*pi*lat/360.)*cos(delta)*cos(h) 
        sza   = (360/(2*pi))*acos(cost) 

    END FUNCTION GET_SZA

    !==========================================================================

    FUNCTION IS_LEAP_YEAR(Year) RESULT(Its_a_Leap_Year)

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=sint) :: Year
        LOGICAL            :: Its_a_Leap_Year

        Its_a_Leap_Year = .FALSE.

        IF ( ( MOD( Year, 4 ) == 0     .AND. &
               MOD( Year, 100 ) /= 0 ) .OR.  &
               MOD( Year, 400 ) == 0 )       &
                Its_a_Leap_Year = .TRUE.

    END FUNCTION IS_LEAP_YEAR

    !==========================================================================

    FUNCTION NUMBER_OF_DAYS(Month,Year) RESULT(last_day)

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER, PARAMETER :: n_months = 12
        INTEGER, PARAMETER :: days_per_month(n_months) = &
                              (/ 31, 28, 31, 30, 31, 30, &
                                 31, 31, 30, 31, 30, 31 /)
        INTEGER(KIND=sint) :: Month
        INTEGER(KIND=sint) :: Year
        INTEGER(KIND=sint) :: last_day 

        last_day = days_per_month(Month)
        IF ( IS_LEAP_YEAR(Year) .AND. Month == 2 ) last_day = 29

    END FUNCTION NUMBER_OF_DAYS

    !==========================================================================


END MODULE FUNCS
