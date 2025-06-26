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

# Export functions for use in other scripts
cat("✅ Weather functions loaded\n")