!-------------------------------------------------------------------------------
! Name: funcs.F90
!-------------------------------------------------------------------------------
MODULE FUNCS 

    CONTAINS

    !==========================================================================
    FUNCTION DAY_OF_YEAR( year, month, day ) RESULT(doy)
    !==========================================================================

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

    !==========================================================================
    END FUNCTION
    !==========================================================================



    !==========================================================================
    FUNCTION GET_SZA(doy, hour, minute, lon, lat, xdim, ydim) RESULT(sza)
    !==========================================================================

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

    !==========================================================================
    END FUNCTION
    !==========================================================================



    !==========================================================================
    FUNCTION IS_LEAP_YEAR(Year) RESULT(Its_a_Leap_Year)
    !==========================================================================

        USE COMMON_CONSTANTS

        IMPLICIT NONE

        INTEGER(KIND=sint) :: Year
        LOGICAL            :: Its_a_Leap_Year

        Its_a_Leap_Year = .FALSE.

        IF ( ( MOD( Year, 4 ) == 0     .AND. &
               MOD( Year, 100 ) /= 0 ) .OR.  &
               MOD( Year, 400 ) == 0 )       &
                Its_a_Leap_Year = .TRUE.

    !==========================================================================
    END FUNCTION IS_LEAP_YEAR
    !==========================================================================



    !==========================================================================
    FUNCTION NUMBER_OF_DAYS(Month,Year) RESULT(last_day)
    !==========================================================================

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

    !==========================================================================
    END FUNCTION NUMBER_OF_DAYS
    !==========================================================================


END MODULE FUNCS
