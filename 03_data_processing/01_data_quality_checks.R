# Updated Quality Check Functions - Uses Fixed World Bank Files
# This replaces the functions in 03_data_processing/01_data_quality_checks.R

library(tidyverse)
library(lubridate)
source("99_utilities/helper_functions.R")

# ======================================================================
# UPDATED WORLD BANK DATA QUALITY CHECKS (USES FIXED FILES)
# ======================================================================

# Updated climate data quality check - prioritizes fixed files
check_climate_data_quality <- function(city_folder) {
  # Check for fixed file first, then fall back to original
  climate_file_fixed <- file.path("data/cities", city_folder, "country_climate_data_fixed.csv")
  climate_file_original <- file.path("data/cities", city_folder, "country_climate_data.csv")
  
  # Determine which file to use
  if (file.exists(climate_file_fixed)) {
    climate_file <- climate_file_fixed
    file_type <- "fixed"
  } else if (file.exists(climate_file_original)) {
    climate_file <- climate_file_original
    file_type <- "original"
  } else {
    return(list(status = "missing", message = "Climate data file not found"))
  }
  
  tryCatch({
    climate_data <- read_csv(climate_file, show_col_types = FALSE)
    
    # Check if it's long format (has indicator_id) or wide format
    if ("indicator_id" %in% colnames(climate_data)) {
      # Long format - can analyze properly
      total_indicators <- length(unique(climate_data$indicator_id))
      total_rows <- nrow(climate_data)
      date_range <- range(climate_data$date, na.rm = TRUE)
      missing_values <- sum(is.na(climate_data$value))
      
      # Indicator summary
      indicator_summary <- climate_data %>%
        group_by(indicator_id) %>%
        summarise(
          data_points = n(),
          missing_values = sum(is.na(value)),
          latest_value = last(value, order_by = date),
          latest_year = max(date, na.rm = TRUE),
          .groups = "drop"
        )
      
      return(list(
        status = "success",
        file_used = paste(file_type, "format"),
        total_indicators = total_indicators,
        total_rows = total_rows,
        date_range = date_range,
        missing_values = missing_values,
        completeness_percent = round((total_rows - missing_values) / total_rows * 100, 1),
        indicators = indicator_summary
      ))
      
    } else {
      # Wide format - need to convert or indicate issue
      indicator_columns <- colnames(climate_data)[grepl("^[A-Z]{2}\\.[A-Z]{2,3}\\.", colnames(climate_data))]
      
      if (length(indicator_columns) > 0) {
        return(list(
          status = "needs_conversion",
          file_used = paste(file_type, "format (wide)"),
          message = paste("Found", length(indicator_columns), "indicators in wide format - needs conversion"),
          indicators_found = indicator_columns
        ))
      } else {
        return(list(status = "error", message = "No indicator columns found"))
      }
    }
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Error reading climate data:", e$message)))
  })
}

# Updated economic data quality check - prioritizes fixed files
check_economic_data_quality <- function(city_folder) {
  # Check for fixed file first, then fall back to original
  economic_file_fixed <- file.path("data/cities", city_folder, "economic_gender_data_fixed.csv")
  economic_file_original <- file.path("data/cities", city_folder, "economic_gender_data.csv")
  
  # Determine which file to use
  if (file.exists(economic_file_fixed)) {
    economic_file <- economic_file_fixed
    file_type <- "fixed"
  } else if (file.exists(economic_file_original)) {
    economic_file <- economic_file_original
    file_type <- "original"
  } else {
    return(list(status = "missing", message = "Economic data file not found"))
  }
  
  tryCatch({
    economic_data <- read_csv(economic_file, show_col_types = FALSE)
    
    # Check if it's long format (has indicator_id) or wide format
    if ("indicator_id" %in% colnames(economic_data)) {
      # Long format - can analyze properly
      total_indicators <- length(unique(economic_data$indicator_id))
      total_rows <- nrow(economic_data)
      missing_values <- sum(is.na(economic_data$value))
      
      # Category breakdown if available
      if ("indicator_category" %in% colnames(economic_data)) {
        category_summary <- economic_data %>%
          group_by(indicator_category) %>%
          summarise(
            indicators = n_distinct(indicator_id),
            data_points = n(),
            missing_values = sum(is.na(value)),
            .groups = "drop"
          )
      } else {
        category_summary <- NULL
      }
      
      return(list(
        status = "success",
        file_used = paste(file_type, "format"),
        total_indicators = total_indicators,
        total_rows = total_rows,
        missing_values = missing_values,
        completeness_percent = round((total_rows - missing_values) / total_rows * 100, 1),
        categories = category_summary
      ))
      
    } else {
      # Wide format - need to convert
      indicator_columns <- colnames(economic_data)[grepl("^[A-Z]{2}\\.[A-Z]{2,3}\\.", colnames(economic_data))]
      
      if (length(indicator_columns) > 0) {
        return(list(
          status = "needs_conversion",
          file_used = paste(file_type, "format (wide)"),
          message = paste("Found", length(indicator_columns), "indicators in wide format - needs conversion"),
          indicators_found = indicator_columns
        ))
      } else {
        return(list(status = "error", message = "No indicator columns found"))
      }
    }
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Error reading economic data:", e$message)))
  })
}

# ======================================================================
# UPDATED WEATHER DATA COMPLETENESS CALCULATION
# ======================================================================

# Fixed weather data quality check (corrects completeness calculation)
check_weather_data_quality <- function(city_folder) {
  weather_file <- file.path("data/cities", city_folder, "weather_data_nasa.csv")
  
  if (!file.exists(weather_file)) {
    return(list(status = "missing", message = "Weather data file not found"))
  }
  
  tryCatch({
    weather_data <- read_csv(weather_file, show_col_types = FALSE)
    
    # Basic checks
    total_rows <- nrow(weather_data)
    date_range <- range(weather_data$date, na.rm = TRUE)
    missing_temp <- sum(is.na(weather_data$temp_avg_c))
    missing_precip <- sum(is.na(weather_data$precipitation_mm))
    
    # Temperature checks
    temp_stats <- list(
      avg_temp = round(mean(weather_data$temp_avg_c, na.rm = TRUE), 1),
      min_temp = round(min(weather_data$temp_min_c, na.rm = TRUE), 1),
      max_temp = round(max(weather_data$temp_max_c, na.rm = TRUE), 1),
      temp_range = round(max(weather_data$temp_max_c, na.rm = TRUE) - 
                           min(weather_data$temp_min_c, na.rm = TRUE), 1)
    )
    
    # Precipitation checks
    precip_stats <- list(
      total_precip = round(sum(weather_data$precipitation_mm, na.rm = TRUE), 1),
      avg_precip = round(mean(weather_data$precipitation_mm, na.rm = TRUE), 2),
      max_daily_precip = round(max(weather_data$precipitation_mm, na.rm = TRUE), 1),
      dry_days = sum(weather_data$precipitation_mm == 0, na.rm = TRUE)
    )
    
    # FIXED: Data completeness calculation
    # Count total possible data points (temp + precip for each day)
    total_possible_points <- total_rows * 2
    missing_points <- missing_temp + missing_precip
    completeness <- round((total_possible_points - missing_points) / total_possible_points * 100, 1)
    
    return(list(
      status = "success",
      total_rows = total_rows,
      date_range = date_range,
      missing_data = list(temperature = missing_temp, precipitation = missing_precip),
      temperature = temp_stats,
      precipitation = precip_stats,
      completeness_percent = completeness
    ))
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Error reading weather data:", e$message)))
  })
}

# ======================================================================
# ENHANCED CITY QUALITY REPORT (USES UPDATED FUNCTIONS)
# ======================================================================

# Generate enhanced quality report for a single city
generate_city_quality_report <- function(city_folder) {
  print_header(paste("ENHANCED QUALITY REPORT:", str_to_upper(str_replace_all(city_folder, "_", " "))))
  
  # Check each data type using updated functions
  weather_quality <- check_weather_data_quality(city_folder)
  climate_quality <- check_climate_data_quality(city_folder)
  economic_quality <- check_economic_data_quality(city_folder)
  social_quality <- check_social_data_quality(city_folder)  # Original function works fine
  
  # Weather data report
  cat("🌡️  WEATHER DATA (NASA POWER):\n")
  if (weather_quality$status == "success") {
    cat("   ✅ Status: Available\n")
    cat("   📅 Date range:", as.character(weather_quality$date_range[1]), "to", 
        as.character(weather_quality$date_range[2]), "\n")
    cat("   📊 Total records:", weather_quality$total_rows, "\n")
    cat("   🌡️  Temperature: Avg", weather_quality$temperature$avg_temp, "°C, Range", 
        weather_quality$temperature$temp_range, "°C\n")
    cat("   🌧️  Precipitation: Total", weather_quality$precipitation$total_precip, 
        "mm,", weather_quality$precipitation$dry_days, "dry days\n")
    cat("   ✅ Completeness:", weather_quality$completeness_percent, "%\n")
  } else {
    cat("   ❌ Status:", weather_quality$status, "-", weather_quality$message, "\n")
  }
  
  # Climate data report (enhanced)
  cat("\n🌍 CLIMATE INDICATORS (World Bank):\n")
  if (climate_quality$status == "success") {
    cat("   ✅ Status: Available (", climate_quality$file_used, ")\n")
    cat("   📊 Indicators:", climate_quality$total_indicators, "\n")
    cat("   📅 Date range:", as.character(climate_quality$date_range[1]), "to", 
        as.character(climate_quality$date_range[2]), "\n")
    cat("   ✅ Completeness:", climate_quality$completeness_percent, "%\n")
    cat("   📋 Working indicators:\n")
    for (i in 1:min(5, nrow(climate_quality$indicators))) {
      ind <- climate_quality$indicators[i, ]
      cat("      •", ind$indicator_id, "(", ind$data_points, "data points)\n")
    }
  } else if (climate_quality$status == "needs_conversion") {
    cat("   ⚠️  Status: Data available but needs format conversion\n")
    cat("   📊 Indicators found:", length(climate_quality$indicators_found), "\n")
    cat("   🔧 Action needed: Run fix_all_worldbank_data()\n")
  } else {
    cat("   ❌ Status:", climate_quality$status, "-", climate_quality$message, "\n")
  }
  
  # Economic data report (enhanced)
  cat("\n💰 ECONOMIC/GENDER DATA (World Bank):\n")
  if (economic_quality$status == "success") {
    cat("   ✅ Status: Available (", economic_quality$file_used, ")\n")
    cat("   📊 Indicators:", economic_quality$total_indicators, "\n")
    cat("   ✅ Completeness:", economic_quality$completeness_percent, "%\n")
    if (!is.null(economic_quality$categories)) {
      cat("   📋 Categories:\n")
      for (i in 1:nrow(economic_quality$categories)) {
        cat_data <- economic_quality$categories[i, ]
        cat("      •", cat_data$indicator_category, ":", cat_data$indicators, "indicators\n")
      }
    }
  } else if (economic_quality$status == "needs_conversion") {
    cat("   ⚠️  Status: Data available but needs format conversion\n")
    cat("   📊 Indicators found:", length(economic_quality$indicators_found), "\n")
    cat("   🔧 Action needed: Run fix_all_worldbank_data()\n")
  } else {
    cat("   ❌ Status:", economic_quality$status, "-", economic_quality$message, "\n")
  }
  
  # Social data report (unchanged)
  cat("\n👥 SOCIAL VULNERABILITY DATA:\n")
  if (social_quality$status == "success") {
    cat("   ✅ Status: Available\n")
    cat("   📊 Geographic units:", social_quality$total_rows, "\n")
    cat("   📋 Variables:", social_quality$total_variables, "\n")
    cat("   🗺️  Geographic ID:", ifelse(social_quality$has_geographic_id, "✅", "❌"), "\n")
    cat("   ✅ Overall completeness:", 100 - social_quality$overall_completeness, "%\n")
  } else {
    cat("   ❌ Status:", social_quality$status, "-", social_quality$message, "\n")
  }
  
  # Overall assessment (updated logic)
  datasets_available <- sum(
    weather_quality$status == "success",
    climate_quality$status %in% c("success", "needs_conversion"), 
    economic_quality$status %in% c("success", "needs_conversion"),
    social_quality$status == "success"
  )
  
  cat("\n📋 OVERALL ASSESSMENT:\n")
  cat("   📊 Datasets available:", datasets_available, "out of 4\n")
  
  # Check if any need conversion
  needs_conversion <- any(
    climate_quality$status == "needs_conversion",
    economic_quality$status == "needs_conversion"
  )
  
  if (datasets_available >= 3) {
    if (needs_conversion) {
      cat("   ⚠️  Status: EXCELLENT (after format conversion) - Run fix_all_worldbank_data()\n")
    } else {
      cat("   ✅ Status: EXCELLENT - Ready for vulnerability analysis\n")
    }
  } else if (datasets_available >= 2) {
    cat("   ⚠️  Status: GOOD - Some analysis possible\n")
  } else {
    cat("   ❌ Status: INCOMPLETE - Needs more data for meaningful analysis\n")
  }
  
  cat("\n", rep("=", 60), "\n\n", sep = "")
  
  # Return enhanced summary
  return(list(
    city = city_folder,
    weather = weather_quality,
    climate = climate_quality,
    economic = economic_quality,
    social = social_quality,
    datasets_available = datasets_available,
    needs_conversion = needs_conversion
  ))
}

# ======================================================================
# QUICK CHECK FUNCTION
# ======================================================================

# Quick function to check if fixed files exist
check_fixed_files_status <- function() {
  print_header("CHECKING FIXED FILES STATUS")
  
  cities_dir <- "data/cities"
  city_folders <- list.dirs(cities_dir, full.names = FALSE, recursive = FALSE)
  city_folders <- city_folders[city_folders != ""]
  
  for (city in city_folders) {
    cat("📁", str_to_title(str_replace_all(city, "_", " ")), ":\n")
    
    climate_fixed <- file.exists(file.path(cities_dir, city, "country_climate_data_fixed.csv"))
    economic_fixed <- file.exists(file.path(cities_dir, city, "economic_gender_data_fixed.csv"))
    
    cat("   Climate data (fixed):", ifelse(climate_fixed, "✅", "❌"), "\n")
    cat("   Economic data (fixed):", ifelse(economic_fixed, "✅", "❌"), "\n")
    
    if (!climate_fixed || !economic_fixed) {
      cat("   🔧 Need to run: fix_all_worldbank_data()\n")
    }
    cat("\n")
  }
}

cat("✅ Updated quality check functions loaded!\n")
cat("🔧 These functions now prioritize fixed World Bank files\n")
cat("📋 New functions available:\n")
cat("  • check_fixed_files_status() - Check if fixed files exist\n")
cat("  • assess_all_cities_quality() - Now uses fixed files when available\n")
cat("\n🚀 Quick commands:\n")
cat("  check_fixed_files_status()     # See if you need to run the fix\n")
cat("  fix_all_worldbank_data()       # Fix the format if needed\n")
cat("  assess_all_cities_quality()    # Re-run quality assessment\n")