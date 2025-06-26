# Climate Safe Cities: Diagnostic and Fixes for Data Collection Issues

library(tidyverse)

# ======================================================================
# DIAGNOSTIC FUNCTIONS
# ======================================================================

# Function to examine actual World Bank data structure
diagnose_worldbank_data <- function(city_folder) {
  # Check climate data
  climate_file <- file.path("data/cities", city_folder, "country_climate_data.csv")
  economic_file <- file.path("data/cities", city_folder, "economic_gender_data.csv")
  
  cat("=== DIAGNOSING WORLD BANK DATA FOR", toupper(city_folder), "===\n\n")
  
  if (file.exists(climate_file)) {
    cat("📊 CLIMATE DATA STRUCTURE:\n")
    climate_data <- read_csv(climate_file, show_col_types = FALSE)
    cat("  Columns:", paste(colnames(climate_data), collapse = ", "), "\n")
    cat("  Rows:", nrow(climate_data), "\n")
    cat("  Sample data:\n")
    print(head(climate_data, 3))
    cat("\n")
  } else {
    cat("❌ Climate data file not found\n\n")
  }
  
  if (file.exists(economic_file)) {
    cat("💰 ECONOMIC DATA STRUCTURE:\n")
    economic_data <- read_csv(economic_file, show_col_types = FALSE)
    cat("  Columns:", paste(colnames(economic_data), collapse = ", "), "\n")
    cat("  Rows:", nrow(economic_data), "\n")
    cat("  Sample data:\n")
    print(head(economic_data, 3))
    cat("\n")
  } else {
    cat("❌ Economic data file not found\n\n")
  }
}

# Function to check what cities were actually attempted vs succeeded
diagnose_collection_gaps <- function() {
  cat("=== DIAGNOSING COLLECTION GAPS ===\n\n")
  
  # Check RDS files in chunks directory
  chunks_dir <- "data/chunks"
  if (dir.exists(chunks_dir)) {
    rds_files <- list.files(chunks_dir, pattern = "\\.rds$", full.names = TRUE)
    cat("📁 Found", length(rds_files), "RDS files in chunks directory:\n")
    
    for (file in rds_files) {
      cat("  •", basename(file), "\n")
      
      # Try to load and examine
      tryCatch({
        data <- readRDS(file)
        if (is.list(data)) {
          if (length(data) > 0) {
            cat("    Contains:", length(data), "items\n")
            if (basename(file) != "final_summary.rds") {
              cat("    Cities:", paste(names(data), collapse = ", "), "\n")
            }
          } else {
            cat("    ⚠️  Empty list\n")
          }
        }
      }, error = function(e) {
        cat("    ❌ Error reading file:", e$message, "\n")
      })
    }
  } else {
    cat("❌ No chunks directory found\n")
  }
  
  cat("\n")
}

# ======================================================================
# FIXED WORLD BANK DATA QUALITY CHECKS
# ======================================================================

# Updated function that handles different World Bank column names
check_worldbank_data_fixed <- function(data_file, data_type = "climate") {
  if (!file.exists(data_file)) {
    return(list(status = "missing", message = paste(data_type, "data file not found")))
  }
  
  tryCatch({
    data <- read_csv(data_file, show_col_types = FALSE)
    
    cat("🔍 Examining", data_type, "data columns:\n")
    cat("   Available columns:", paste(colnames(data), collapse = ", "), "\n")
    
    # Look for various possible indicator ID columns
    possible_id_cols <- c("indicator_id", "indicatorID", "id", "indicator.id", "indicator")
    id_col <- intersect(possible_id_cols, colnames(data))[1]
    
    # Also check for other identifying columns
    if (is.na(id_col)) {
      # Look for any column that might contain indicator codes
      for (col in colnames(data)) {
        sample_values <- head(data[[col]], 3)
        # Check if it looks like World Bank indicator codes (e.g., "EN.CLC.HEAT.XD")
        if (any(grepl("^[A-Z]{2}\\.[A-Z]{3}\\.", sample_values, na.rm = TRUE))) {
          id_col <- col
          cat("   🎯 Found potential indicator column:", col, "\n")
          break
        }
      }
    }
    
    if (is.na(id_col)) {
      cat("   ❌ No indicator ID column found among:", paste(possible_id_cols, collapse = ", "), "\n")
      cat("   📋 Available columns for inspection:\n")
      for (i in 1:min(5, ncol(data))) {
        col_name <- colnames(data)[i]
        sample_vals <- head(data[[col_name]], 3)
        cat("      ", col_name, ":", paste(sample_vals, collapse = ", "), "\n")
      }
      return(list(status = "error", message = "No indicator ID column found", columns = colnames(data)))
    }
    
    # If we found an ID column, proceed with analysis
    cat("   ✅ Using indicator column:", id_col, "\n")
    
    total_indicators <- length(unique(data[[id_col]]))
    total_rows <- nrow(data)
    missing_values <- sum(is.na(data$value))
    
    # Get list of indicators
    indicators_list <- unique(data[[id_col]])
    
    cat("   📊 Analysis results:\n")
    cat("      Total indicators:", total_indicators, "\n")
    cat("      Total rows:", total_rows, "\n")
    cat("      Missing values:", missing_values, "\n")
    cat("      Indicators found:\n")
    for (ind in head(indicators_list, 10)) {  # Show first 10
      cat("        •", ind, "\n")
    }
    if (length(indicators_list) > 10) {
      cat("        ... and", length(indicators_list) - 10, "more\n")
    }
    
    return(list(
      status = "success",
      id_column = id_col,
      total_indicators = total_indicators,
      total_rows = total_rows,
      missing_values = missing_values,
      indicators = indicators_list,
      completeness_percent = round((total_rows - missing_values) / total_rows * 100, 1)
    ))
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Error reading", data_type, "data:", e$message)))
  })
}

# ======================================================================
# COMPREHENSIVE DIAGNOSTIC REPORT
# ======================================================================

# Run comprehensive diagnostics
run_comprehensive_diagnostics <- function() {
  cat("============================================================\n")
  cat("  COMPREHENSIVE DIAGNOSTICS FOR DATA COLLECTION ISSUES\n")
  cat("============================================================\n\n")
  
  # 1. Check collection gaps
  diagnose_collection_gaps()
  
  # 2. Check World Bank data structure for available cities
  cities_with_data <- c("new_york", "los_angeles", "london", "tokyo")
  
  for (city in cities_with_data) {
    if (dir.exists(file.path("data/cities", city))) {
      diagnose_worldbank_data(city)
      
      # Test fixed diagnostic functions
      climate_file <- file.path("data/cities", city, "country_climate_data.csv")
      economic_file <- file.path("data/cities", city, "economic_gender_data.csv")
      
      if (file.exists(climate_file)) {
        cat("🔧 TESTING FIXED CLIMATE DATA ANALYSIS:\n")
        climate_result <- check_worldbank_data_fixed(climate_file, "climate")
        cat("   Status:", climate_result$status, "\n")
        if (climate_result$status == "success") {
          cat("   ✅ Successfully identified", climate_result$total_indicators, "indicators\n")
        }
        cat("\n")
      }
      
      if (file.exists(economic_file)) {
        cat("🔧 TESTING FIXED ECONOMIC DATA ANALYSIS:\n")
        economic_result <- check_worldbank_data_fixed(economic_file, "economic")
        cat("   Status:", economic_result$status, "\n")
        if (economic_result$status == "success") {
          cat("   ✅ Successfully identified", economic_result$total_indicators, "indicators\n")
        }
        cat("\n")
      }
      
      cat(rep("-", 60), "\n\n")
    }
  }
  
  # 3. Weather data completeness issue
  cat("🌡️  WEATHER DATA COMPLETENESS CHECK:\n")
  for (city in cities_with_data) {
    weather_file <- file.path("data/cities", city, "weather_data_nasa.csv")
    if (file.exists(weather_file)) {
      weather_data <- read_csv(weather_file, show_col_types = FALSE)
      
      # Check actual completeness
      total_possible_days <- as.numeric(max(weather_data$date) - min(weather_data$date)) + 1
      actual_days <- nrow(weather_data)
      temp_missing <- sum(is.na(weather_data$temp_avg_c))
      precip_missing <- sum(is.na(weather_data$precipitation_mm))
      
      cat("   ", str_to_title(str_replace_all(city, "_", " ")), ":\n")
      cat("     Expected days:", total_possible_days, "\n")
      cat("     Actual records:", actual_days, "\n")
      cat("     Coverage:", round(actual_days / total_possible_days * 100, 1), "%\n")
      cat("     Missing temperature:", temp_missing, "records\n")
      cat("     Missing precipitation:", precip_missing, "records\n")
      
      # The issue: completeness calculation was wrong
      correct_completeness <- round((actual_days * 2 - temp_missing - precip_missing) / (actual_days * 2) * 100, 1)
      cat("     Correct completeness:", correct_completeness, "%\n\n")
    }
  }
}

# ======================================================================
# QUICK FIXES
# ======================================================================

# Quick fix for World Bank data issues
create_fixed_quality_check <- function() {
  cat("💡 CREATING FIXED QUALITY CHECK FUNCTIONS...\n")
  
  # Create updated quality check functions
  fixed_code <- '
# FIXED: Climate data quality check
check_climate_data_quality_fixed <- function(city_folder) {
  climate_file <- file.path("data/cities", city_folder, "country_climate_data.csv")
  
  if (!file.exists(climate_file)) {
    return(list(status = "missing", message = "Climate data file not found"))
  }
  
  tryCatch({
    climate_data <- read_csv(climate_file, show_col_types = FALSE)
    
    # Find indicator ID column - more flexible approach
    possible_id_cols <- c("indicator_id", "indicatorID", "id", "indicator.id", "indicator")
    id_col <- NA
    
    for (col in possible_id_cols) {
      if (col %in% colnames(climate_data)) {
        id_col <- col
        break
      }
    }
    
    # If standard columns not found, look for columns with indicator-like patterns
    if (is.na(id_col)) {
      for (col in colnames(climate_data)) {
        sample_values <- head(climate_data[[col]], 3)
        if (any(grepl("^[A-Z]{2}\\\\.[A-Z]{3}\\\\.", sample_values, na.rm = TRUE))) {
          id_col <- col
          break
        }
      }
    }
    
    if (is.na(id_col)) {
      return(list(status = "error", message = "No indicator ID column found", 
                  available_columns = colnames(climate_data)))
    }
    
    # Proceed with analysis using found column
    total_indicators <- length(unique(climate_data[[id_col]]))
    total_rows <- nrow(climate_data)
    date_range <- range(climate_data$date, na.rm = TRUE)
    missing_values <- sum(is.na(climate_data$value))
    
    return(list(
      status = "success",
      id_column_used = id_col,
      total_indicators = total_indicators,
      total_rows = total_rows,
      date_range = date_range,
      missing_values = missing_values,
      completeness_percent = round((total_rows - missing_values) / total_rows * 100, 1)
    ))
    
  }, error = function(e) {
    return(list(status = "error", message = paste("Error reading climate data:", e$message)))
  })
}
'

# Write to a temporary file for manual inspection
writeLines(fixed_code, "temp_fixed_functions.R")
cat("✅ Fixed functions written to temp_fixed_functions.R\n")
cat("📝 Review and integrate these fixes into your quality check script\n")
}

# ======================================================================
# MAIN DIAGNOSTIC EXECUTION
# ======================================================================

cat("🔍 DIAGNOSTIC FUNCTIONS LOADED\n")
cat("📋 Available diagnostic commands:\n")
cat("  • run_comprehensive_diagnostics() - Full diagnostic report\n")
cat("  • diagnose_worldbank_data('city_folder') - Check specific city WB data\n")
cat("  • diagnose_collection_gaps() - Check what was actually collected\n")
cat("  • create_fixed_quality_check() - Generate fixes for quality check\n")
cat("\n🚀 Start with: run_comprehensive_diagnostics()\n")