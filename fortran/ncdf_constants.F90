!-------------------------------------------------------------------------------
! Name: ncdf_constants.F90
!-------------------------------------------------------------------------------
MODULE NCDF_CONSTANTS 

    USE COMMON_CONSTANTS

    IMPLICIT NONE 

    ! global attributes
    CHARACTER(LEN=*), PARAMETER :: DWD = "Deutscher Wetterdienst"
    CHARACTER(LEN=*), PARAMETER :: DWD_URL = "http://www.dwd.de"
    CHARACTER(LEN=*), PARAMETER :: ESA_URL = "http://www.esa-cloud-cci.info"
    CHARACTER(LEN=*), PARAMETER :: DWD_MAIL = "contact.cloudcci@dwd.de"
    CHARACTER(LEN=*), PARAMETER :: PROJECT = "project"
    CHARACTER(LEN=*), PARAMETER :: PROJECT_STR = "Climate Change Initiative - European Space Agency"
    CHARACTER(LEN=*), PARAMETER :: TITLE = "title"
    CHARACTER(LEN=*), PARAMETER :: TITLE_STR = "Evaluation of ERA-Interim clouds"
    CHARACTER(LEN=*), PARAMETER :: INS = "institution"
    CHARACTER(LEN=*), PARAMETER :: SRC = "Source"
    CHARACTER(LEN=*), PARAMETER :: SRC_STR = "ERA-Interim"
    CHARACTER(LEN=*), PARAMETER :: TCS = "time_coverage_start"
    CHARACTER(LEN=*), PARAMETER :: TCE = "time_coverage_end"
    CHARACTER(LEN=*), PARAMETER :: TCR = "time_coverage_resolution"
    CHARACTER(LEN=*), PARAMETER :: TCR_STR = "P1M"
    CHARACTER(LEN=*), PARAMETER :: THV = "cot_thv"
    CHARACTER(LEN=*), PARAMETER :: NOF = "number_of_files"
    CHARACTER(LEN=*), PARAMETER :: SCO = "scops"
    CHARACTER(LEN=*), PARAMETER :: MPC = "mpc"
    CHARACTER(LEN=*), PARAMETER :: RES = "spatial_resolution"
    CHARACTER(LEN=*), PARAMETER :: RES_STR = "0.50 degree"
    CHARACTER(LEN=*), PARAMETER :: DTC = "date_created"
    CHARACTER(LEN=*), PARAMETER :: CRM = "creator_email"
    CHARACTER(LEN=*), PARAMETER :: CRU = "creator_url"
    CHARACTER(LEN=*), PARAMETER :: CRE = "created_name"
    CHARACTER(LEN=*), PARAMETER :: XMIN = "geospatial_lon_min"
    CHARACTER(LEN=*), PARAMETER :: XMAX = "geospatial_lon_max"
    CHARACTER(LEN=*), PARAMETER :: YMIN = "geospatial_lat_min"
    CHARACTER(LEN=*), PARAMETER :: YMAX = "geospatial_lat_max"
    CHARACTER(LEN=*), PARAMETER :: LONRES = "geospatial_lon_resolution"
    CHARACTER(LEN=*), PARAMETER :: LATRES = "geospatial_lat_resolution"
    CHARACTER(LEN=*), PARAMETER :: LONUNI = "geospatial_lon_units"
    CHARACTER(LEN=*), PARAMETER :: LONUNI_STR = "degrees_east" 
    CHARACTER(LEN=*), PARAMETER :: LATUNI = "geospatial_lat_units"
    CHARACTER(LEN=*), PARAMETER :: LATUNI_STR = "degrees_north"
    CHARACTER(LEN=*), PARAMETER :: CDMGRD = "cdm_data_type"
    CHARACTER(LEN=*), PARAMETER :: GRID = "Grid"
    CHARACTER(LEN=*), PARAMETER :: REFERENCES = "refernces"
    CHARACTER(LEN=*), PARAMETER :: SUMMARY = "summary"
    CHARACTER(LEN=*), PARAMETER :: SUMMARY_TXT = &
            "This dataset contains monthly Level-3 global cloud property products "//&
            "derived from the cloud simulator based on ERA-Interim reanalysis data. "//&
            "Averaged onto a regular grid."
    CHARACTER(LEN=*), PARAMETER :: KEYWORDS = "keywords"
    CHARACTER(LEN=*), PARAMETER :: KEYWORDS_TXT = &
            "EARTH SCIENCE > ATMOSPHERE > MODELS > REANALYSIS DATASET > "//&
            "CLOUD SIMULATOR > CLOUDS > CLOUD PROPERTIES"
    CHARACTER(LEN=*), PARAMETER :: COMMENT = "comment"
    CHARACTER(LEN=*), PARAMETER :: COMMENT_TXT = &
            "These data were produced at ESACCI as part of the ESA Cloud CCI project."
    CHARACTER(LEN=*), PARAMETER :: LICENSE = "license"
    CHARACTER(LEN=*), PARAMETER :: LICENSE_TXT = &
            "ESA CCI Data Policy: free and open access"


    ! attributes
    CHARACTER(LEN=*), PARAMETER :: fill   = "_FillValue"
    CHARACTER(LEN=*), PARAMETER :: long   = "long_name"
    CHARACTER(LEN=*), PARAMETER :: units  = "units"
    CHARACTER(LEN=*), PARAMETER :: fscale = "scale_factor"
    CHARACTER(LEN=*), PARAMETER :: offset = "add_offset"
    CHARACTER(LEN=*), PARAMETER :: vmin   = "valid_min"
    CHARACTER(LEN=*), PARAMETER :: vmax   = "valid_max"

    ! units
    CHARACTER(LEN=*), PARAMETER :: unit_one  = "1"
    CHARACTER(LEN=*), PARAMETER :: unit_ctp  = "hPa"
    CHARACTER(LEN=*), PARAMETER :: unit_cwp  = "g/m2"
    CHARACTER(LEN=*), PARAMETER :: unit_cer  = "um"
    CHARACTER(LEN=*), PARAMETER :: unit_ctt  = "K"
    CHARACTER(LEN=*), PARAMETER :: unit_cth  = "km"

    ! long names
    CHARACTER(LEN=*), PARAMETER :: axis_str = " histogram bin border values"
    CHARACTER(LEN=*), PARAMETER :: bins_str = " histogram bin centres"
    CHARACTER(LEN=*), PARAMETER :: jointstr = "joint histogram of cloud optical thickness and cloud top pressure"
    CHARACTER(LEN=*), PARAMETER :: cot_hist_str = "histogram of cloud optical thickness"
    CHARACTER(LEN=*), PARAMETER :: ctp_hist_str = "histogram of cloud top pressure"
    CHARACTER(LEN=*), PARAMETER :: ctt_hist_str = "histogram of cloud top temperature"
    CHARACTER(LEN=*), PARAMETER :: cer_hist_str = "histogram of cloud effective radius"
    CHARACTER(LEN=*), PARAMETER :: cwp_hist_str = "histogram of cloud water path"
    CHARACTER(LEN=*), PARAMETER :: phase_bins_str = "phase histogram bins"

    CHARACTER(LEN=*), PARAMETER :: cfc_str = "cloud fraction"
    CHARACTER(LEN=*), PARAMETER :: cph_str = "fraction of liquid water clouds"
    CHARACTER(LEN=*), PARAMETER :: cph_day_str = "daytime fraction of liquid water cloud"
    CHARACTER(LEN=*), PARAMETER :: ctp_str = "cloud top pressure"
    CHARACTER(LEN=*), PARAMETER :: ctt_str = "cloud top temperature"
    CHARACTER(LEN=*), PARAMETER :: cth_str = "cloud top height"
    CHARACTER(LEN=*), PARAMETER :: cot_str = "cloud optical thickness"
    CHARACTER(LEN=*), PARAMETER :: cot_liq_str = "liquid water cloud optical thickness"
    CHARACTER(LEN=*), PARAMETER :: cot_ice_str = "ice water cloud optical thickness"
    CHARACTER(LEN=*), PARAMETER :: cer_str = "cloud effective radius"
    CHARACTER(LEN=*), PARAMETER :: cer_liq_str = "liquid water cloud effective radius"
    CHARACTER(LEN=*), PARAMETER :: cer_ice_str = "ice water cloud effective radius"
    CHARACTER(LEN=*), PARAMETER :: cwp_str = "cloud water path"
    CHARACTER(LEN=*), PARAMETER :: lwp_str = "cloud liquid water path"
    CHARACTER(LEN=*), PARAMETER :: iwp_str = "cloud ice water path"
    CHARACTER(LEN=*), PARAMETER :: cwp_allsky_str = "grid box mean of cloud water path"
    CHARACTER(LEN=*), PARAMETER :: lwp_allsky_str = "grid box mean of cloud liquid water path"
    CHARACTER(LEN=*), PARAMETER :: iwp_allsky_str = "grid box mean of cloud ice water path"
    CHARACTER(LEN=*), PARAMETER :: nobs_ctp_str = "number of cloud observations"
    CHARACTER(LEN=*), PARAMETER :: nobs_lwp_str = "number of lwp observations"
    CHARACTER(LEN=*), PARAMETER :: nobs_iwp_str = "number of iwp observations"

END MODULE NCDF_CONSTANTS
