!-------------------------------------------------------------------------------
! Name: common_constants.F90
!-------------------------------------------------------------------------------
MODULE COMMON_CONSTANTS 

    IMPLICIT NONE 

    ! Type KIND value
    INTEGER, PARAMETER :: byte = 1
    INTEGER, PARAMETER :: sint = 2 
    INTEGER, PARAMETER :: lint = 4
    INTEGER, PARAMETER :: sREAL = 4
    INTEGER, PARAMETER :: dREAL = 8

    INTEGER, PARAMETER :: file_length = 512
    INTEGER, PARAMETER :: path_length = 2048

    ! fill values
    INTEGER(KIND=sint),  PARAMETER :: sint_fill_value = -32767
    INTEGER(KIND=lint),  PARAMETER :: lint_fill_value = -999
    REAL(KIND=sreal),    PARAMETER :: sreal_fill_value = -999.0
    REAL(KIND=dreal),    PARAMETER :: dreal_fill_value = -999.0

    ! Mathematical constants
    REAL(KIND=sreal), PARAMETER :: pi = 3.14159265
    REAL(KIND=sreal), PARAMETER :: d2r = pi/180.0
    REAL(KIND=sreal), PARAMETER :: r_specific = 287.058 ![J/(kg*K)]
    REAL(KIND=sreal), PARAMETER :: zd_sea = 0.33
    REAL(KIND=sreal), PARAMETER :: zd_land = 0.43
    REAL(KIND=sreal), PARAMETER :: zntot_sea = 100.
    REAL(KIND=sreal), PARAMETER :: zntot_land = 300.
    REAL(KIND=sreal), PARAMETER :: rtt = 273.15
    REAL(KIND=sreal), PARAMETER :: zrefde = 0.64952
    REAL(KIND=sreal), PARAMETER :: rho_water = 1000. ![kg/m3]
    REAL(KIND=sreal), PARAMETER :: rho_ice = 916.7   ![kg/m3]
    REAL(KIND=sreal), PARAMETER :: qext_water = 2.0
    REAL(KIND=sreal), PARAMETER :: qext_ice = 2.1

    ! Specific gas constants of dry air and water vapor
    ! computing geopotential profile, plevel & pressure difference
    REAL(KIND=sreal), PARAMETER :: r_dry_air = 287.05
    REAL(KIND=sreal), PARAMETER :: r_water_vap = 461.51
    REAL(KIND=sreal), PARAMETER :: dither = 1.0E-3
    ! 1  = z = 64.56 km
    ! 60 = z = 0.01 km
    ! 61 = z = 0 km
    REAL(KIND=sreal), PARAMETER :: avec(61) = &
         [0.000000,     2.000000E+01, 3.842534E+01, 6.364780E+01, &
         9.563696E+01, 1.344833E+02, 1.805844E+02, 2.347791E+02, &
         2.984958E+02, 3.739719E+02, 4.646182E+02, 5.756511E+02, &
         7.132180E+02, 8.836604E+02, 1.094835E+03, 1.356475E+03, &
         1.680640E+03, 2.082274E+03, 2.579889E+03, 3.196422E+03, &
         3.960292E+03, 4.906707E+03, 6.018020E+03, 7.306633E+03, &
         8.765055E+03, 1.037612E+04, 1.207745E+04, 1.377532E+04, &
         1.537980E+04, 1.681947E+04, 1.804518E+04, 1.902770E+04, &
         1.975511E+04, 2.022220E+04, 2.042986E+04, 2.038448E+04, &
         2.009740E+04, 1.958433E+04, 1.886475E+04, 1.796136E+04, &
         1.689947E+04, 1.570645E+04, 1.441112E+04, 1.304322E+04, &
         1.163276E+04, 1.020950E+04, 8.802355E+03, 7.438805E+03, &
         6.144316E+03, 4.941777E+03, 3.850913E+03, 2.887697E+03, &
         2.063780E+03, 1.385913E+03, 8.553618E+02, 4.673335E+02, &
         2.103939E+02, 6.588924E+01, 7.367743,     0.000000,     &
         0.000000 ]
    REAL(KIND=sreal), PARAMETER :: bvec(61) = &
         [0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
         0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
         0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
         0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
         0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
         0.0000000,     0.0000000,     0.0000000,     0.0000000,     &
         7.5823496E-05, 4.6139490E-04, 1.8151561E-03, 5.0811172E-03, &
         1.1142910E-02, 2.0677876E-02, 3.4121163E-02, 5.1690407E-02, &
         7.3533833E-02, 9.9674702E-02, 0.1300225,     0.1643843,     &
         0.2024759,     0.2439331,     0.2883230,     0.3351549,     &
         0.3838921,     0.4339629,     0.4847715,     0.5357099,     &
         0.5861684,     0.6355475,     0.6832686,     0.7287858,     &
         0.7715966,     0.8112534,     0.8473749,     0.8796569,     &
         0.9078839,     0.9319403,     0.9518215,     0.9676452,     &
         0.9796627,     0.9882701,     0.9940194,     0.9976301,     &
         1.0000000 ]

    ! land sea mask
    INTEGER(KIND=sint), PARAMETER :: sea = 0
    INTEGER(KIND=sint), PARAMETER :: land = 1

    ! sim_core.F90
    INTEGER(KIND=sint), PARAMETER :: is_night = 0
    INTEGER(KIND=sint), PARAMETER :: is_day = 1

    ! cosp scops overlap type (1=max, 2=rand, 3=max/rand)
    INTEGER(KIND=sint), PARAMETER :: dwd_scops = 1
    INTEGER(KIND=sint), PARAMETER :: cosp_scops = 2
    INTEGER(KIND=sint), PARAMETER :: over_max = 1 !not used in dwd_scops
    INTEGER(KIND=sint), PARAMETER :: over_rand = 2
    INTEGER(KIND=sint), PARAMETER :: over_max_rand = 3
    INTEGER(KIND=sint), PARAMETER :: no_mixed_phase = 1 !for dwd_scops
    INTEGER(KIND=sint), PARAMETER :: mixed_phase = 2    !for dwd_scops
    INTEGER(KIND=sint), PARAMETER :: cwc_mod_off = 1
    INTEGER(KIND=sint), PARAMETER :: cwc_mod_on = 2 !binning approach

    INTEGER(KIND=sint), PARAMETER :: max_cot = 100.0
    REAL(KIND=sreal),   PARAMETER :: is_clear = 0.0
    REAL(KIND=sreal),   PARAMETER :: is_cloud = 1.0
    REAL(KIND=sreal),   PARAMETER :: is_ice = 0.0
    REAL(KIND=sreal),   PARAMETER :: is_liq = 1.0

    ! GET_MEAN in funcs.F90
    INTEGER(KIND=sint), PARAMETER :: normal = 0
    INTEGER(KIND=sint), PARAMETER :: allsky = 1

    ! histogram definitions
    INTEGER(KIND=sint), PARAMETER :: n_hist_phase = 2
    INTEGER(KIND=sint), PARAMETER :: liq_bin = 1
    INTEGER(KIND=sint), PARAMETER :: ice_bin = 2
    ! 2d histogram definitions: number of axis = borders
    INTEGER(KIND=sint), PARAMETER :: n_hist_cot = 14
    INTEGER(KIND=sint), PARAMETER :: n_hist_ctp = 16
    ! 1d histogram definitions, number of bins
    INTEGER(KIND=sint), PARAMETER :: n_cot_bins = 14
    INTEGER(KIND=sint), PARAMETER :: n_ctp_bins = 15
    INTEGER(KIND=sint), PARAMETER :: n_ctt_bins = 16
    INTEGER(KIND=sint), PARAMETER :: n_cwp_bins = 14
    INTEGER(KIND=sint), PARAMETER :: n_cer_bins = 11 

END MODULE COMMON_CONSTANTS
