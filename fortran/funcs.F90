!-------------------------------------------------------------------------------
! Name: funcs.F90
!-------------------------------------------------------------------------------
MODULE FUNCS 

    CONTAINS


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
