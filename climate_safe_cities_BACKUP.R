# Synthesized Climate Safe Cities R Script
# Combines existing functionality with dual weather data strategy

# ======================================================================
# PACKAGE INSTALLATION AND SETUP
# ======================================================================

# Install main packages for API access (if not already installed)
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("tidyverse")) install.packages("tidyverse")

# Install API-specific packages
if (!require("wbstats")) install.packages("wbstats")
if (!require("tidycensus")) install.packages("tidycensus")

# NASA POWER (primary weather data source)
if (!require("nasapower")) install.packages("nasapower")

# OpenWeatherMap API alternative (optional enhancement)
if (!require("ROpenWeatherMap")) install.packages("ROpenWeatherMap")

# Additional useful packages for weather data
if (!require("weathercan")) {
  if (!require("remotes")) install.packages("remotes")
  remotes::install_github("ropensci/weathercan")  # For Canadian data
}

# Create project directories (uncomment if needed)
# dir.create("climate_vulnerability_analysis", showWarnings = FALSE)
# dir.create("data", showWarnings = FALSE)
# dir.create("code", showWarnings = FALSE)
# dir.create("results", showWarnings = FALSE)

# Function to properly set up all API keys
setup_all_api_keys <- function() {
  cat("=== SETTING UP ALL API KEYS ===\n\n")
  
  # 1. Census API Key
  census_key <- Sys.getenv("CENSUS_KEY")
  if (census_key != "") {
    library(tidycensus)
    census_api_key(census_key, install = TRUE, overwrite = TRUE)
    cat("✓ Census API key configured\n")
  } else {
    cat("✗ Census API key not found in environment\n")
  }
  
  # 2. OpenWeatherMap API Key
  openweather_key <- Sys.getenv("OPENWEATHER_KEY")
  if (openweather_key != "") {
    cat("✓ OpenWeatherMap API key found:", substr(openweather_key, 1, 8), "...\n")
  } else {
    cat("✗ OpenWeatherMap API key not found\n")
  }
  
  # 3. Check .Renviron file exists and has keys
  if (file.exists(".Renviron")) {
    renviron_content <- readLines(".Renviron")
    has_census <- any(grepl("CENSUS_KEY", renviron_content))
    has_openweather <- any(grepl("OPENWEATHER_KEY", renviron_content))
    
    cat("\n.Renviron file status:\n")
    cat("  Census key in file:", has_census, "\n")
    cat("  OpenWeather key in file:", has_openweather, "\n")
  } else {
    cat("\n✗ .Renviron file not found\n")
  }
  
  cat("\n=== API KEY SETUP COMPLETE ===\n")
}

# ======================================================================
# WEATHER DATA FUNCTIONS - DUAL SOURCE STRATEGY
# ======================================================================

library(tidyverse)
library(lubridate)
library(nasapower)
library(httr)
library(jsonlite)

# Primary NASA POWER Functions
get_temperature_data_nasa <- function(lat, lon, start_date, end_date) {
  cat("Fetching temperature data from NASA POWER for coordinates:", lat, ",", lon, "\n")
  
  tryCatch({
    temp_data <- get_power(
      community = "ag",
      lonlat = c(lon, lat),
      pars = c("T2M_MAX", "T2M_MIN", "T2M"),
      dates = c(start_date, end_date),
      temporal_api = "daily"
    )
    
    clean_data <- temp_data %>%
      select(YEAR, MM, DD, T2M_MAX, T2M_MIN, T2M) %>%
      mutate(
        date = ymd(paste(YEAR, MM, DD, sep = "-")),
        temp_max_c = T2M_MAX,
        temp_min_c = T2M_MIN,
        temp_avg_c = T2M
      ) %>%
      select(date, temp_max_c, temp_min_c, temp_avg_c) %>%
      filter(!is.na(date))
    
    return(clean_data)
    
  }, error = function(e) {
    message(paste("Error retrieving NASA POWER data:", e$message))
    return(NULL)
  })
}

get_precipitation_data_nasa <- function(lat, lon, start_date, end_date) {
  cat("Fetching precipitation data from NASA POWER for coordinates:", lat, ",", lon, "\n")
  
  tryCatch({
    precip_data <- get_power(
      community = "ag",
      lonlat = c(lon, lat),
      pars = c("PRECTOTCORR"),
      dates = c(start_date, end_date),
      temporal_api = "daily"
    )
    
    clean_data <- precip_data %>%
      select(YEAR, MM, DD, PRECTOTCORR) %>%
      mutate(
        date = ymd(paste(YEAR, MM, DD, sep = "-")),
        precipitation_mm = PRECTOTCORR
      ) %>%
      select(date, precipitation_mm) %>%
      filter(!is.na(date))
    
    return(clean_data)
    
  }, error = function(e) {
    message(paste("Error retrieving NASA POWER precipitation data:", e$message))
    return(NULL)
  })
}

get_city_weather_data <- function(city_name, lat, lon, start_year, end_year) {
  start_date <- paste0(start_year, "-01-01")
  end_date <- paste0(end_year, "-12-31")
  
  cat("Collecting weather data for", city_name, "from", start_year, "to", end_year, "\n")
  
  temp_data <- get_temperature_data_nasa(lat, lon, start_date, end_date)
  precip_data <- get_precipitation_data_nasa(lat, lon, start_date, end_date)
  
  if (!is.null(temp_data) && !is.null(precip_data)) {
    combined_data <- temp_data %>%
      left_join(precip_data, by = "date") %>%
      mutate(city_name = city_name)
    
    return(combined_data)
  } else {
    message("Failed to retrieve complete weather data")
    return(NULL)
  }
}

# Enhanced OpenWeatherMap Functions (NEW)
get_openweather_historical <- function(lat, lon, start_date, end_date, api_key) {
  cat("Fetching OpenWeatherMap data for coordinates:", lat, ",", lon, "\n")
  
  if (is.null(api_key) || api_key == "") {
    message("No OpenWeatherMap API key provided, skipping OWM data")
    return(NULL)
  }
  
  # Test API key first with a simple current weather call
  test_response <- tryCatch({
    GET(
      url = "https://api.openweathermap.org/data/2.5/weather",
      query = list(
        lat = lat,
        lon = lon,
        appid = api_key,
        units = "metric"
      )
    )
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(test_response) || status_code(test_response) != 200) {
    message(paste("OpenWeatherMap API key test failed. Status:", 
                  ifelse(is.null(test_response), "Connection Error", status_code(test_response))))
    message("Skipping OpenWeatherMap data collection.")
    return(NULL)
  }
  
  cat("OpenWeatherMap API key validated successfully!\n")
  
  # If we get here, the API key works - proceed with historical data
  tryCatch({
    # For free tier, we're limited to 5 days of historical data
    # Sample a few recent dates
    end_date_obj <- as.Date(end_date)
    sample_dates <- seq(end_date_obj - 30, end_date_obj, by = "7 days")[1:5]
    
    all_data <- map_dfr(sample_dates, function(date) {
      timestamp <- as.numeric(as.POSIXct(date, tz = "UTC"))
      
      response <- GET(
        url = "https://api.openweathermap.org/data/2.5/onecall/timemachine",
        query = list(
          lat = lat,
          lon = lon,
          dt = timestamp,
          appid = api_key,
          units = "metric"
        )
      )
      
      if (status_code(response) == 200) {
        data <- content(response, "parsed")
        current <- data$current
        
        tibble(
          date = as.Date(as.POSIXct(current$dt, origin = "1970-01-01", tz = "UTC")),
          temp_c = current$temp,
          feels_like_c = current$feels_like,
          humidity = current$humidity,
          pressure = current$pressure,
          wind_speed = current$wind_speed,
          precipitation_mm = ifelse(is.null(current$rain), 0, 
                                    ifelse(is.null(current$rain$`1h`), 0, current$rain$`1h`)),
          source = "OpenWeatherMap"
        )
      } else {
        message(paste("API call failed for date", date, "- Status:", status_code(response)))
        NULL
      }
    })
    
    return(all_data)
    
  }, error = function(e) {
    message(paste("Error retrieving OpenWeatherMap historical data:", e$message))
    return(NULL)
  })
}

# Master comprehensive weather function
get_comprehensive_weather_data <- function(city_name, lat, lon, start_year, end_year, openweather_key = NULL) {
  cat("Collecting comprehensive weather data for", city_name, "\n")
  
  start_date <- paste0(start_year, "-01-01")
  end_date <- paste0(end_year, "-12-31")
  
  # 1. Get NASA POWER data (primary)
  cat("1. Fetching NASA POWER data...\n")
  nasa_temp <- get_power(
    community = "ag",
    lonlat = c(lon, lat),
    pars = c("T2M_MAX", "T2M_MIN", "T2M", "RH2M"),
    dates = c(start_date, end_date),
    temporal_api = "daily"
  )
  
  nasa_precip <- get_power(
    community = "ag",
    lonlat = c(lon, lat),
    pars = c("PRECTOTCORR"),
    dates = c(start_date, end_date),
    temporal_api = "daily"
  )
  
  # Process NASA POWER data
  nasa_data <- nasa_temp %>%
    left_join(nasa_precip, by = c("YEAR", "MM", "DD")) %>%
    mutate(
      date = ymd(paste(YEAR, MM, DD, sep = "-")),
      temp_max_c = T2M_MAX,
      temp_min_c = T2M_MIN,
      temp_avg_c = T2M,
      humidity = RH2M,
      precipitation_mm = PRECTOTCORR,
      source = "NASA_POWER"
    ) %>%
    select(date, temp_max_c, temp_min_c, temp_avg_c, humidity, precipitation_mm, source) %>%
    filter(!is.na(date))
  
  # 2. Get OpenWeatherMap data (optional enhancement)
  openweather_data <- NULL
  if (!is.null(openweather_key) && openweather_key != "") {
    cat("2. Fetching OpenWeatherMap data...\n")
    openweather_data <- get_openweather_historical(lat, lon, start_date, end_date, openweather_key)
  } else {
    cat("2. Skipping OpenWeatherMap (no API key provided)\n")
  }
  
  # 3. Combine results
  result <- list(
    nasa_power = nasa_data,
    openweather = openweather_data,
    city_info = list(
      name = city_name,
      lat = lat,
      lon = lon,
      data_years = paste(start_year, "-", end_year)
    )
  )
  
  # 4. Calculate urban indicators if both sources available
  if (!is.null(openweather_data) && nrow(openweather_data) > 0) {
    cat("3. Calculating urban climate indicators...\n")
    
    overlap_data <- nasa_data %>%
      inner_join(openweather_data, by = "date", suffix = c("_nasa", "_owm"))
    
    if (nrow(overlap_data) > 0) {
      urban_indicators <- overlap_data %>%
        summarise(
          temp_difference_avg = mean(temp_c - temp_avg_c, na.rm = TRUE),
          humidity_difference_avg = mean(humidity_owm - humidity_nasa, na.rm = TRUE),
          data_points = n(),
          .groups = "drop"
        ) %>%
        mutate(
          urban_heat_island_effect = temp_difference_avg,
          humidity_bias = humidity_difference_avg
        )
      
      result$urban_indicators <- urban_indicators
    }
  }
  
  return(result)
}

# ======================================================================
# CITY DATABASE (Enhanced from your existing)
# ======================================================================

city_database <- tribble(
  ~city_name,    ~country_name, ~country_iso, ~lat, ~lon, ~noaa_station, ~state_code, ~county,
  "New York",    "United States", "USA", 40.7128, -74.0060, "GHCND:USW00094728", "NY", "New York County",
  "Los Angeles", "United States", "USA", 34.0522, -118.2437, "GHCND:USW00023174", "CA", "Los Angeles County", 
  "London",      "United Kingdom", "GBR", 51.5074, -0.1278, "GHCND:UK000056225", NA, NA,
  "Tokyo",       "Japan", "JPN", 35.6762, 139.6503, "GHCND:JA000047662", NA, NA,
  "Mumbai",      "India", "IND", 19.0760, 72.8777, "GHCND:IN012001000", NA, NA,
  "Sao Paulo",   "Brazil", "BRA", -23.5505, -46.6333, "GHCND:BR021839900", NA, NA,
  "Shanghai",    "China", "CHN", 31.2304, 121.4737, "GHCND:CH000058367", NA, NA,
  "Cairo",       "Egypt", "EGY", 30.0444, 31.2357, "GHCND:EG000062366", NA, NA,
  "Sydney",      "Australia", "AUS", -33.8688, 151.2093, "GHCND:ASN00066062", NA, NA,
  "Lagos",       "Nigeria", "NGA", 6.5244, 3.3792, "GHCND:NI000065201", NA, NA
)

# ======================================================================
# WORLD BANK AND CENSUS FUNCTIONS (From your existing script)
# ======================================================================

library(wbstats)
library(tidycensus)


get_country_climate_data <- function(country_iso, start_year, end_year) {
  # Working indicators only (based on your test results)
  climate_indicators <- c(
    "EN.CLC.HEAT.XD",        # Heat index exposure ✓ Working
    "AG.LND.PRCP.MM",        # Average precipitation ✓ Working  
    "SP.URB.TOTL.IN.ZS",     # Urban population ✓ Working
    "EG.ELC.ACCS.ZS",        # Access to electricity ✓ Working
    "ER.LND.PTLD.ZS",        # Terrestrial protected areas ✓ Working
    # Added alternative CO2/emissions indicators
    "EN.ATM.CO2E.KT",        # CO2 emissions (kt) - alternative to per capita
    "EN.ATM.METH.KT.CE",     # Methane emissions (kt of CO2 equivalent)
    "EN.ATM.NOXE.KT.CE"      # Nitrous oxide emissions (kt of CO2 equivalent)
  )
  
  tryCatch({
    wb_data(
      indicator = climate_indicators,
      country = country_iso,
      start_date = start_year,
      end_date = end_year
    )
  }, error = function(e) {
    message(paste("Error in World Bank batch retrieval, trying individually..."))
    
    # Try indicators one by one
    working_data <- data.frame()
    
    for (ind in climate_indicators) {
      ind_data <- tryCatch({
        cat("Trying indicator:", ind, "\n")
        wb_data(
          indicator = ind,
          country = country_iso,
          start_date = start_year,
          end_date = end_year
        )
      }, error = function(err) {
        cat("  Failed:", ind, "\n")
        return(NULL)
      })
      
      if (!is.null(ind_data) && nrow(ind_data) > 0) {
        cat("  Success:", ind, "- got", nrow(ind_data), "rows\n")
        working_data <- bind_rows(working_data, ind_data)
      }
    }
    
    return(working_data)
  })
}

get_economic_and_gender_data <- function(country_iso, start_year, end_year) {
  # Core economic indicators (verified to work)
  economic_indicators <- c(
    "NY.GDP.MKTP.CD",         # GDP (current US$)
    "NY.GDP.PCAP.CD",         # GDP per capita (current US$)
    "NY.GDP.PCAP.PP.CD",      # GDP per capita, PPP
    "NV.IND.MANF.ZS",         # Manufacturing, value added (% of GDP)
    "NV.SRV.TOTL.ZS",         # Services, value added (% of GDP)
    "NV.AGR.TOTL.ZS"          # Agriculture, value added (% of GDP)
  )
  
  # Simplified gender indicators (most reliable ones)
  gender_employment <- c(
    "SL.TLF.CACT.FE.ZS",      # Labor force participation rate, female
    "SL.TLF.CACT.FM.ZS",      # Ratio of female to male labor force participation
    "SL.UEM.TOTL.FE.ZS"       # Unemployment, female
  )
  
  gender_education <- c(
    "SE.PRM.CMPT.FE.ZS",      # Primary completion rate, female
    "SE.ADT.LITR.FE.ZS",      # Literacy rate, adult female
    "SE.ENR.TERT.FM.ZS"       # Ratio of female to male tertiary enrollment
  )
  
  gender_health <- c(
    "SP.DYN.LE00.FE.IN",      # Life expectancy at birth, female
    "SH.STA.MMRT",            # Maternal mortality ratio
    "SP.ADO.TFRT"             # Adolescent fertility rate
  )
  
  gender_rights <- c(
    "SG.GEN.PARL.ZS"          # Proportion of seats held by women in parliament
    # Removed problematic indicators: SG.DMK.SRCR.FN, etc.
  )
  
  # Combine indicators
  all_indicators <- c(economic_indicators, gender_employment, gender_education, gender_health, gender_rights)
  
  # Try to get data with individual fallback
  combined_data <- tryCatch({
    wb_data(
      indicator = all_indicators,
      country = country_iso,
      start_date = start_year,
      end_date = end_year
    )
  }, error = function(e) {
    cat("Batch retrieval failed, trying indicators individually...\n")
    
    all_data <- data.frame()
    
    for (ind in all_indicators) {
      ind_data <- tryCatch({
        wb_data(
          indicator = ind,
          country = country_iso,
          start_date = start_year,
          end_date = end_year
        )
      }, error = function(err) {
        return(NULL)
      })
      
      if (!is.null(ind_data) && nrow(ind_data) > 0) {
        all_data <- bind_rows(all_data, ind_data)
      }
    }
    
    return(all_data)
  })
  
  # Add categories
  if (!is.null(combined_data) && nrow(combined_data) > 0) {
    id_col <- intersect(c("indicator_id", "indicatorID", "id"), colnames(combined_data))[1]
    
    if (!is.na(id_col)) {
      combined_data <- combined_data %>%
        mutate(indicator_category = case_when(
          .data[[id_col]] %in% economic_indicators ~ "Economic",
          .data[[id_col]] %in% gender_employment ~ "Employment",
          .data[[id_col]] %in% gender_education ~ "Education",
          .data[[id_col]] %in% gender_health ~ "Health",
          .data[[id_col]] %in% gender_rights ~ "Rights",
          TRUE ~ "Other"
        ))
    }
  }
  
  return(combined_data)
}

# Social vulnerability functions
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

get_global_social_vulnerability <- function(country_iso, start_year, end_year) {
  social_indicators <- c(
    "SI.POV.GINI",
    "SP.POP.65UP.TO.ZS",
    "SP.URB.TOTL.IN.ZS",
    "SP.POP.TOTL",
    "SP.DYN.LE00.IN",
    "SH.STA.BASS.ZS",
    "SH.H2O.BASW.ZS",
    "SE.ADT.LITR.ZS"
  )
  
  social_data <- tryCatch({
    wb_data(
      indicator = social_indicators,
      country = country_iso,
      start_date = start_year,
      end_date = end_year
    )
  }, error = function(e) {
    print(paste("Error in social data retrieval:", e$message))
    
    all_data <- data.frame()
    for (ind in social_indicators) {
      ind_data <- tryCatch({
        wb_data(
          indicator = ind,
          country = country_iso,
          start_date = start_year,
          end_date = end_year
        )
      }, error = function(err) {
        return(NULL)
      })
      
      if (!is.null(ind_data) && nrow(ind_data) > 0) {
        all_data <- bind_rows(all_data, ind_data)
      }
    }
    
    if (nrow(all_data) > 0) {
      return(all_data)
    } else {
      return(NULL)
    }
  })
  
  return(social_data)
}

# ======================================================================
# ENHANCED MASTER COLLECTION FUNCTION
# ======================================================================

collect_city_climate_vulnerability_enhanced <- function(city_name, years, use_openweather = FALSE) {
  # Look up city information
  city_info <- city_database %>% filter(city_name == !!city_name)
  
  if (nrow(city_info) == 0) {
    message(paste("Error: City", city_name, "not found in database."))
    return(NULL)
  }
  
  # Extract city information
  country_iso <- city_info$country_iso
  country_name <- city_info$country_name
  lat <- city_info$lat
  lon <- city_info$lon
  state_code <- city_info$state_code
  county <- city_info$county
  
  message(paste("Collecting enhanced climate vulnerability data for", city_name, "in", country_name))
  
  # Create directory
  city_dir <- file.path("data", "cities", tolower(gsub(" ", "_", city_name)))
  dir.create(city_dir, showWarnings = FALSE, recursive = TRUE)
  
  city_data <- list()
  
  # 1. Enhanced weather data (NASA POWER + optional OpenWeatherMap)
  tryCatch({
    message("Collecting enhanced weather data...")
    
    if (use_openweather) {
      openweather_key <- Sys.getenv("OPENWEATHER_KEY")
      weather_data <- get_comprehensive_weather_data(
        city_name = city_name,
        lat = lat,
        lon = lon,
        start_year = min(years),
        end_year = max(years),
        openweather_key = if(openweather_key != "") openweather_key else NULL
      )
    } else {
      # Use simplified NASA POWER only
      weather_data <- list(
        nasa_power = get_city_weather_data(city_name, lat, lon, min(years), max(years))
      )
    }
    
    if (!is.null(weather_data$nasa_power) && nrow(weather_data$nasa_power) > 0) {
      write_csv(weather_data$nasa_power, file.path(city_dir, "weather_data_nasa.csv"))
      message("NASA POWER weather data saved successfully!")
      city_data$weather <- weather_data
      
      # Save OpenWeatherMap data if available
      if (!is.null(weather_data$openweather)) {
        write_csv(weather_data$openweather, file.path(city_dir, "weather_data_openweathermap.csv"))
        message("OpenWeatherMap data saved successfully!")
      }
      
      # Save urban indicators if available
      if (!is.null(weather_data$urban_indicators)) {
        write_csv(weather_data$urban_indicators, file.path(city_dir, "urban_climate_indicators.csv"))
        message("Urban climate indicators saved successfully!")
      }
    }
  }, error = function(e) {
    message(paste("Error collecting weather data:", e$message))
  })
  
  # 2. Country-level climate data (World Bank)
  tryCatch({
    message("Collecting country-level climate data...")
    climate_data <- get_country_climate_data(
      country_iso = country_iso,
      start_year = min(years),
      end_year = max(years)
    )
    
    if (!is.null(climate_data) && nrow(climate_data) > 0) {
      write_csv(climate_data, file.path(city_dir, "country_climate_data.csv"))
      message("Country climate data saved successfully!")
      city_data$climate <- climate_data
    }
  }, error = function(e) {
    message(paste("Error collecting climate data:", e$message))
  })
  
  # 3. Economic and gender data
  tryCatch({
    message("Collecting economic and gender data...")
    economic_data <- get_economic_and_gender_data(
      country_iso = country_iso,
      start_year = min(years),
      end_year = max(years)
    )
    
    if (!is.null(economic_data) && nrow(economic_data) > 0) {
      write_csv(economic_data, file.path(city_dir, "economic_gender_data.csv"))
      message("Economic and gender data saved successfully!")
      city_data$economic <- economic_data
    }
  }, error = function(e) {
    message(paste("Error collecting economic data:", e$message))
  })
  
  # 4. Social vulnerability data
  tryCatch({
    message("Collecting social vulnerability data...")
    
    if (country_iso == "USA" && !is.na(state_code)) {
      latest_available_census_year <- min(max(years[years <= 2021]), 2021)
      
      if (!is.na(county)) {
        social_data <- get_us_social_vulnerability(
          state_code = state_code,
          county = county,
          year = latest_available_census_year
        )
      } else {
        social_data <- get_us_social_vulnerability(
          state_code = state_code,
          year = latest_available_census_year
        )
      }
    } else {
      social_data <- get_global_social_vulnerability(
        country_iso = country_iso,
        start_year = min(years),
        end_year = max(years)
      )
    }
    
    if (!is.null(social_data) && nrow(social_data) > 0) {
      write_csv(social_data, file.path(city_dir, "social_vulnerability_data.csv"))
      message("Social vulnerability data saved successfully!")
      city_data$social <- social_data
    }
  }, error = function(e) {
    message(paste("Error collecting social vulnerability data:", e$message))
  })
  
  # 5. Create summary file
  summary_data <- tibble(
    city_name = city_name,
    country_name = country_name,
    country_iso = country_iso,
    data_years = paste(min(years), "-", max(years)),
    weather_data_available = !is.null(city_data$weather),
    climate_data_available = !is.null(city_data$climate),
    economic_data_available = !is.null(city_data$economic),
    social_data_available = !is.null(city_data$social),
    openweather_enhanced = use_openweather && !is.null(city_data$weather$openweather),
    urban_indicators_available = !is.null(city_data$weather$urban_indicators)
  )
  
  write_csv(summary_data, file.path(city_dir, "data_summary.csv"))
  
  message(paste("Enhanced data collection complete for", city_name))
  return(city_data)
}

# ======================================================================
# BATCH PROCESSING FUNCTIONS
# ======================================================================

# Function to process multiple cities with enhanced data collection
collect_multi_city_data_enhanced <- function(city_list = NULL, max_data_year = NULL, 
                                             lookback_years = 10, use_openweather = FALSE) {
  # Determine the latest year of available data
  if (is.null(max_data_year)) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    max_data_year <- current_year - 2
    message(paste0("No max_data_year specified. Using estimated latest available data year: ", 
                   max_data_year, " (current year minus 2)"))
  } else {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    if (max_data_year > current_year) {
      max_data_year <- current_year - 1
      message(paste0("max_data_year adjusted to ", max_data_year, 
                     " as data for future years is not available"))
    }
  }
  
  # Calculate year range
  min_data_year <- max_data_year - (lookback_years - 1)
  years <- min_data_year:max_data_year
  
  message(paste0("Analyzing data for years ", min_data_year, " to ", max_data_year))
  
  # If no city list provided, use all cities in database
  if (is.null(city_list)) {
    city_list <- city_database$city_name
  }
  
  # Create a directory for comparative analysis
  comparative_dir <- file.path("data", "comparative")
  dir.create(comparative_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Process each city
  all_city_data <- list()
  
  for (city in city_list) {
    print(paste("Processing city:", city))
    city_data <- collect_city_climate_vulnerability_enhanced(city, years, use_openweather)
    all_city_data[[city]] <- city_data
    
    # Pause to avoid hitting API rate limits
    Sys.sleep(2)
  }
  
  # Create comparative datasets
  print("Creating comparative datasets across cities...")
  
  # Extract weather data for all cities (NASA POWER)
  weather_comparative <- map_dfr(names(all_city_data), function(city) {
    if (!is.null(all_city_data[[city]]$weather) && !is.null(all_city_data[[city]]$weather$nasa_power)) {
      all_city_data[[city]]$weather$nasa_power %>%
        mutate(city_name = city)
    }
  })
  
  if (nrow(weather_comparative) > 0) {
    write_csv(weather_comparative, file.path(comparative_dir, "weather_comparative_nasa.csv"))
    print("Comparative NASA POWER weather data saved!")
  }
  
  # Extract OpenWeatherMap data if available
  if (use_openweather) {
    openweather_comparative <- map_dfr(names(all_city_data), function(city) {
      if (!is.null(all_city_data[[city]]$weather) && !is.null(all_city_data[[city]]$weather$openweather)) {
        all_city_data[[city]]$weather$openweather %>%
          mutate(city_name = city)
      }
    })
    
    if (nrow(openweather_comparative) > 0) {
      write_csv(openweather_comparative, file.path(comparative_dir, "weather_comparative_openweather.csv"))
      print("Comparative OpenWeatherMap data saved!")
    }
  }
  
  # Extract climate data for all cities
  climate_comparative <- map_dfr(names(all_city_data), function(city) {
    if (!is.null(all_city_data[[city]]$climate)) {
      all_city_data[[city]]$climate %>%
        mutate(city_name = city)
    }
  })
  
  if (nrow(climate_comparative) > 0) {
    write_csv(climate_comparative, file.path(comparative_dir, "climate_comparative.csv"))
    print("Comparative climate data saved!")
  }
  
  # Extract economic data for all cities
  economic_comparative <- map_dfr(names(all_city_data), function(city) {
    if (!is.null(all_city_data[[city]]$economic)) {
      all_city_data[[city]]$economic %>%
        mutate(city_name = city)
    }
  })
  
  if (nrow(economic_comparative) > 0) {
    write_csv(economic_comparative, file.path(comparative_dir, "economic_comparative.csv"))
    print("Comparative economic data saved!")
  }
  
  print("Enhanced multi-city data collection complete!")
  return(all_city_data)
}

# ======================================================================
# TESTING AND EXAMPLE USAGE
# ======================================================================

# Test the enhanced system with a single city
test_enhanced_analysis <- function() {
  # Test with New York (already tested basic NASA POWER)
  test_city <- "New York"
  city_info <- city_database %>% filter(city_name == test_city)
  
  if (nrow(city_info) > 0) {
    cat("Testing enhanced data collection for", test_city, "\n")
    
    # Test enhanced collection with NASA POWER only
    test_data_basic <- collect_city_climate_vulnerability_enhanced(
      city_name = test_city,
      years = 2020:2022,
      use_openweather = FALSE
    )
    
    if (!is.null(test_data_basic)) {
      cat("Basic enhanced collection successful for", test_city, "\n")
      
      # Test with OpenWeatherMap if API key is available
      openweather_key <- Sys.getenv("OPENWEATHER_KEY")
      if (openweather_key != "") {
        cat("Testing with OpenWeatherMap enhancement...\n")
        test_data_enhanced <- collect_city_climate_vulnerability_enhanced(
          city_name = test_city,
          years = 2020:2022,
          use_openweather = TRUE
        )
        
        if (!is.null(test_data_enhanced$weather$openweather)) {
          cat("OpenWeatherMap enhancement successful!\n")
        }
      } else {
        cat("No OpenWeatherMap API key found, skipping enhanced test\n")
      }
    }
  }
}

# Run test
test_enhanced_analysis()

# Example: Collect data for a subset of cities with basic NASA POWER
example_cities_basic <- c("New York", "London", "Tokyo")
basic_analysis <- collect_multi_city_data_enhanced(
  city_list = example_cities_basic,
  max_data_year = 2022,
  lookback_years = 5,
  use_openweather = FALSE
)

# Create a summary of data availability
city_summary <- map_dfr(example_cities_basic, function(city) {
  summary_file <- file.path("data", "cities", tolower(gsub(" ", "_", city)), "data_summary.csv")
  if (file.exists(summary_file)) {
    read_csv(summary_file, show_col_types = FALSE)
  }
})

# Save the overall summary
if (nrow(city_summary) > 0) {
  write_csv(city_summary, file.path("data", "enhanced_cities_data_summary.csv"))
  print("Enhanced cities data summary saved!")
  print(city_summary)
} else {
  print("No data summary available.")
}

# ======================================================================
# USAGE INSTRUCTIONS AND NEXT STEPS
# ======================================================================

cat("
=== ENHANCED CLIMATE SAFE CITIES ANALYSIS ===

WHAT'S NEW IN THIS SYNTHESIZED VERSION:

1. DUAL WEATHER DATA STRATEGY:
   - NASA POWER (primary): Global, consistent, free
   - OpenWeatherMap (optional): Urban precision, validation

2. ENHANCED FUNCTIONS:
   - get_comprehensive_weather_data(): Combines both sources
   - collect_city_climate_vulnerability_enhanced(): Full city analysis
   - collect_multi_city_data_enhanced(): Batch processing

3. URBAN CLIMATE INDICATORS:
   - Urban heat island detection when both sources available
   - Temperature and humidity bias calculations
   - City-specific vs regional climate differences

IMMEDIATE NEXT STEPS:

1. BASIC ANALYSIS (NASA POWER ONLY):
   - Run: collect_multi_city_data_enhanced(use_openweather = FALSE)
   - This gives you solid baseline for all cities

2. ENHANCED ANALYSIS (ADD OPENWEATHERMAP):
   - Get free API key: https://openweathermap.org/api
   - Add OPENWEATHER_KEY=your_key to .Renviron
   - Run: collect_multi_city_data_enhanced(use_openweather = TRUE)
   - Focus on 3-5 key cities due to API limits

3. DATA ANALYSIS PHASE:
   - All data saved in organized city folders
   - Comparative datasets in data/comparative/
   - Ready for vulnerability index calculation

RECOMMENDED WORKFLOW:
Phase 1: Run basic analysis for all 10 cities (NASA POWER)
Phase 2: Add OpenWeatherMap for 3-5 priority cities  
Phase 3: Develop climate vulnerability index
Phase 4: Build Shiny dashboard

The system is now ready for comprehensive global city climate analysis!
")