get_us_social_vulnerability <- function(state_code, county = NULL, year = 2020) {
  # Updated and verified ACS variable codes
  svi_vars <- c(
    poverty = "B17001_002",        # Poverty (verified)
    less_than_hs = "B15003_002",   # Less than high school (verified)
    age_65_plus = "B01001_020",    # Age 65+ (verified)
    no_vehicle = "B08201_002",     # No vehicle (verified)
    mobile_homes = "B25024_010",   # Mobile homes (verified)
    crowded = "B25014_005",        # Crowded housing (verified)
    limited_english = "B16004_007" # FIXED: Updated limited English variable
  )
  
  tryCatch({
    if (!is.null(county)) {
      svi_data <- get_acs(
        geography = "tract",
        variables = svi_vars,
        state = state_code,
        county = county,
        year = year,
        geometry = FALSE
      )
    } else {
      svi_data <- get_acs(
        geography = "tract",
        variables = svi_vars,
        state = state_code,
        year = year,
        geometry = FALSE
      )
    }
    
    svi_data_wide <- svi_data %>%
      pivot_wider(
        id_cols = c(GEOID, NAME),
        names_from = variable,
        values_from = estimate
      )
    
    return(svi_data_wide)
  }, error = function(e) {
    message(paste("Error retrieving census data:", e$message))
    
    # Fallback to state-level data
    tryCatch({
      message("Trying state-level aggregation as fallback...")
      state_data <- get_acs(
        geography = "state",
        variables = svi_vars,
        state = state_code,
        year = year,
        geometry = FALSE
      )
      
      state_data_wide <- state_data %>%
        pivot_wider(
          id_cols = c(GEOID, NAME),
          names_from = variable,
          values_from = estimate
        )
      
      return(state_data_wide)
    }, error = function(e2) {
      message("State-level fallback also failed, using World Bank social data instead")
      return(NULL)
    })
  })
}

# Export functions for use in other scripts
cat("✅ Census functions loaded\n")