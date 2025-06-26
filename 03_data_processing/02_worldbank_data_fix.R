# Fixed World Bank Data Processing Functions
# Handles the new wide format from World Bank API

library(tidyverse)

# ======================================================================
# CONVERT WORLD BANK DATA FROM WIDE TO LONG FORMAT
# ======================================================================

# Function to convert wide-format World Bank data to expected long format
convert_worldbank_wide_to_long <- function(wide_data, data_type = "climate") {
  # Identify World Bank indicator columns (start with capital letters and contain dots)
  indicator_columns <- colnames(wide_data)[grepl("^[A-Z]{2}\\.[A-Z]{2,3}\\.", colnames(wide_data))]
  
  cat("📊 Found", length(indicator_columns), data_type, "indicators:\n")
  for (ind in indicator_columns) {
    cat("   •", ind, "\n")
  }
  
  if (length(indicator_columns) == 0) {
    return(NULL)
  }
  
  # Convert to long format
  long_data <- wide_data %>%
    select(iso2c, iso3c, country, date, all_of(indicator_columns)) %>%
    pivot_longer(
      cols = all_of(indicator_columns),
      names_to = "indicator_id",
      values_to = "value"
    ) %>%
    filter(!is.na(value))  # Remove missing values
  
  cat("✅ Converted to long format:", nrow(long_data), "rows\n")
  return(long_data)
}

# Function to fix a single city's World Bank data files
fix_city_worldbank_data <- function(city_folder) {
  city_path <- file.path("data/cities", city_folder)
  
  cat("🔧 Fixing World Bank data for", str_to_title(str_replace_all(city_folder, "_", " ")), "\n")
  
  # Fix climate data
  climate_file <- file.path(city_path, "country_climate_data.csv")
  if (file.exists(climate_file)) {
    cat("📊 Processing climate data...\n")
    climate_wide <- read_csv(climate_file, show_col_types = FALSE)
    climate_long <- convert_worldbank_wide_to_long(climate_wide, "climate")
    
    if (!is.null(climate_long)) {
      # Save the fixed version
      write_csv(climate_long, file.path(city_path, "country_climate_data_fixed.csv"))
      cat("✅ Fixed climate data saved\n")
    }
  }
  
  # Fix economic data
  economic_file <- file.path(city_path, "economic_gender_data.csv")
  if (file.exists(economic_file)) {
    cat("💰 Processing economic data...\n")
    economic_wide <- read_csv(economic_file, show_col_types = FALSE)
    economic_long <- convert_worldbank_wide_to_long(economic_wide, "economic")
    
    if (!is.null(economic_long)) {
      # Add indicator categories
      economic_long <- economic_long %>%
        mutate(indicator_category = case_when(
          str_starts(indicator_id, "NY.GDP") ~ "Economic",
          str_starts(indicator_id, "NV.") ~ "Economic",
          str_starts(indicator_id, "SL.") ~ "Employment",
          str_starts(indicator_id, "SE.") ~ "Education",
          str_starts(indicator_id, "SH.") | str_starts(indicator_id, "SP.") ~ "Health",
          str_starts(indicator_id, "SG.") ~ "Rights",
          TRUE ~ "Other"
        ))
      
      write_csv(economic_long, file.path(city_path, "economic_gender_data_fixed.csv"))
      cat("✅ Fixed economic data saved\n")
    }
  }
  
  cat("🎉 World Bank data fix complete for", city_folder, "\n\n")
}

# Function to fix all cities' World Bank data
fix_all_worldbank_data <- function() {
  cities_dir <- "data/cities"
  city_folders <- list.dirs(cities_dir, full.names = FALSE, recursive = FALSE)
  city_folders <- city_folders[city_folders != ""]
  
  cat("🔧 FIXING WORLD BANK DATA FOR ALL CITIES\n")
  cat("=====================================\n\n")
  
  for (city_folder in city_folders) {
    fix_city_worldbank_data(city_folder)
  }
  
  cat("🎉 ALL WORLD BANK DATA FIXES COMPLETE!\n")
  cat("📋 Fixed files created with '_fixed.csv' suffix\n")
  cat("🔄 Re-run quality assessment to see improvements\n")
}

# ======================================================================
# UPDATED QUALITY CHECK FUNCTIONS FOR FIXED DATA
# ======================================================================

# Updated climate data quality check (works with fixed format)
check_climate_data_quality_fixed <- function(city_folder) {
  # Try fixed file first, then original
  climate_file_fixed <- file.path("data/cities", city_folder, "country_climate_data_fixed.csv")
  climate_file_original <- file.path("data/cities", city_folder, "country_climate_data.csv")
  
  climate_file <- if (file.exists(climate_file_fixed)) climate_file_fixed else climate_file_original
  
  if (!file.exists(climate_file)) {
    return(list(status = "missing", message = "Climate data file not found"))
  }
  
  tryCatch({
    climate_data <- read_csv(climate_file, show_col_types = FALSE)
    
    # Check if it's the long format (fixed) or wide format (original)
    if ("indicator_id" %in% colnames(climate_data)) {
      # Long format (fixed)
      total_indicators <- length(unique(climate_data$indicator_id))
      total_rows <- nrow(climate_data)
      date_range <- range(climate_data$date, na.rm = TRUE)
      missing_values <- sum(is.na(climate_data$value))
      
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
        format = "long (fixed)",
        total_indicators = total_indicators,
        total_rows = total_rows,
        date_range = date_range,
        missing_values = missing_values,
        completeness_percent = round((total_rows - missing_values) / total_rows * 100, 1),
        indicators = indicator_summary
      ))
      
    } else {
      # Wide format (original) - try to analyze
      indicator_columns <- colnames(climate_data)[grepl("^[A-Z]{2}\\.[A-Z]{2,3}\\.", colnames(climate_data))]
      
      if (length(indicator_columns) > 0) {
        return(list(
          status = "success",
          format = "wide (needs fixing)",
          total_indicators = length(indicator_columns),
          indicators_found = indicator_columns,
          message = "Data available but needs format conversion"
        ))
      } else {
        return(list(status = "error", message = "No recognizable indicator columns found"))
      }
    }
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Error reading climate data:", e$message)))
  })
}

# Updated economic data quality check (works with fixed format)
check_economic_data_quality_fixed <- function(city_folder) {
  # Try fixed file first, then original
  economic_file_fixed <- file.path("data/cities", city_folder, "economic_gender_data_fixed.csv")
  economic_file_original <- file.path("data/cities", city_folder, "economic_gender_data.csv")
  
  economic_file <- if (file.exists(economic_file_fixed)) economic_file_fixed else economic_file_original
  
  if (!file.exists(economic_file)) {
    return(list(status = "missing", message = "Economic data file not found"))
  }
  
  tryCatch({
    economic_data <- read_csv(economic_file, show_col_types = FALSE)
    
    # Check if it's the long format (fixed) or wide format (original)
    if ("indicator_id" %in% colnames(economic_data)) {
      # Long format (fixed)
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
        format = "long (fixed)",
        total_indicators = total_indicators,
        total_rows = total_rows,
        missing_values = missing_values,
        completeness_percent = round((total_rows - missing_values) / total_rows * 100, 1),
        categories = category_summary
      ))
      
    } else {
      # Wide format (original)
      indicator_columns <- colnames(economic_data)[grepl("^[A-Z]{2}\\.[A-Z]{2,3}\\.", colnames(economic_data))]
      
      if (length(indicator_columns) > 0) {
        return(list(
          status = "success", 
          format = "wide (needs fixing)",
          total_indicators = length(indicator_columns),
          indicators_found = indicator_columns,
          message = "Data available but needs format conversion"
        ))
      } else {
        return(list(status = "error", message = "No recognizable indicator columns found"))
      }
    }
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Error reading economic data:", e$message)))
  })
}

# ======================================================================
# READY-TO-USE COMMANDS
# ======================================================================

cat("🔧 WORLD BANK DATA FIX FUNCTIONS LOADED\n")
cat("📋 Available commands:\n")
cat("  • fix_all_worldbank_data() - Fix all cities' World Bank data format\n")
cat("  • fix_city_worldbank_data('city_folder') - Fix specific city\n")
cat("  • check_climate_data_quality_fixed('city_folder') - Test fixed climate data\n")
cat("  • check_economic_data_quality_fixed('city_folder') - Test fixed economic data\n")
cat("\n🚀 Quick start:\n")
cat("  fix_all_worldbank_data()  # Fix the format issue for all cities\n")