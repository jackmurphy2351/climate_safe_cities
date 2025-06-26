# Climate Safe Cities: Helper Functions
# Utility functions used across multiple modules

# ======================================================================
# API KEY MANAGEMENT
# ======================================================================

# Function to properly set up all API keys
setup_all_api_keys <- function() {
  cat("=== SETTING UP ALL API KEYS ===\n\n")
  
  # 1. Census API Key
  census_key <- Sys.getenv("CENSUS_KEY")
  if (census_key != "") {
    if (requireNamespace("tidycensus", quietly = TRUE)) {
      library(tidycensus)
      census_api_key(census_key, install = TRUE, overwrite = TRUE)
      cat("✓ Census API key configured\n")
    } else {
      cat("⚠ tidycensus package not available\n")
    }
  } else {
    cat("✗ Census API key not found in environment\n")
    cat("  Add CENSUS_KEY=your_key to .Renviron file\n")
  }
  
  # 2. OpenWeatherMap API Key
  openweather_key <- Sys.getenv("OPENWEATHER_KEY")
  if (openweather_key != "") {
    cat("✓ OpenWeatherMap API key found:", substr(openweather_key, 1, 8), "...\n")
  } else {
    cat("✗ OpenWeatherMap API key not found\n")
    cat("  Add OPENWEATHER_KEY=your_key to .Renviron file\n")
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
    cat("  Create .Renviron file in project root with your API keys\n")
  }
  
  cat("\n=== API KEY SETUP COMPLETE ===\n")
}

# Function to check if all required API keys are available
check_api_keys <- function() {
  keys_status <- list(
    census = Sys.getenv("CENSUS_KEY") != "",
    openweather = Sys.getenv("OPENWEATHER_KEY") != ""
  )
  
  return(keys_status)
}

# Function to create .Renviron template if it doesn't exist
create_renviron_template <- function() {
  if (!file.exists(".Renviron")) {
    cat("# Climate Safe Cities API Keys\n", file = ".Renviron")
    cat("# Get Census API key: https://api.census.gov/data/key_signup.html\n", file = ".Renviron", append = TRUE)
    cat("CENSUS_KEY=your_census_api_key_here\n", file = ".Renviron", append = TRUE)
    cat("\n", file = ".Renviron", append = TRUE)
    cat("# Get OpenWeatherMap API key: https://openweathermap.org/api\n", file = ".Renviron", append = TRUE)
    cat("OPENWEATHER_KEY=your_openweather_api_key_here\n", file = ".Renviron", append = TRUE)
    
    cat("✓ Created .Renviron template file\n")
    cat("📝 Please edit .Renviron file to add your actual API keys\n")
    cat("🔄 Restart R session after editing .Renviron\n")
  } else {
    cat("ℹ .Renviron file already exists\n")
  }
}

# ======================================================================
# DATA VALIDATION HELPERS
# ======================================================================

# Check if data frame is valid and not empty
is_valid_data <- function(data, min_rows = 1) {
  if (is.null(data)) return(FALSE)
  if (!is.data.frame(data)) return(FALSE)
  if (nrow(data) < min_rows) return(FALSE)
  return(TRUE)
}

# Safely extract column from data frame
safe_extract <- function(data, column_name, default = NA) {
  if (!is_valid_data(data)) return(default)
  if (!column_name %in% colnames(data)) return(default)
  return(data[[column_name]])
}

# Clean city name for file naming
clean_city_name <- function(city_name) {
  cleaned <- tolower(city_name)
  cleaned <- gsub("[^a-z0-9]", "_", cleaned)
  cleaned <- gsub("_+", "_", cleaned)  # Remove multiple underscores
  cleaned <- gsub("^_|_$", "", cleaned)  # Remove leading/trailing underscores
  return(cleaned)
}

# ======================================================================
# FILE MANAGEMENT HELPERS
# ======================================================================

# Create directory if it doesn't exist
ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    cat("✓ Created directory:", path, "\n")
  }
  return(path)
}

# Safe file save with backup
safe_save <- function(data, filepath, backup = TRUE) {
  if (!is_valid_data(data)) {
    warning("Cannot save invalid data to ", filepath)
    return(FALSE)
  }
  
  # Create directory if needed
  dir_path <- dirname(filepath)
  ensure_directory(dir_path)
  
  # Create backup if file exists
  if (backup && file.exists(filepath)) {
    backup_path <- paste0(filepath, ".backup.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    file.copy(filepath, backup_path)
  }
  
  # Save the file
  tryCatch({
    if (grepl("\\.csv$", filepath)) {
      readr::write_csv(data, filepath)
    } else if (grepl("\\.rds$", filepath)) {
      saveRDS(data, filepath)
    } else {
      warning("Unsupported file format for ", filepath)
      return(FALSE)
    }
    cat("✓ Saved:", filepath, "\n")
    return(TRUE)
  }, error = function(e) {
    warning("Failed to save ", filepath, ": ", e$message)
    return(FALSE)
  })
}

# ======================================================================
# ERROR HANDLING HELPERS
# ======================================================================

# Safely execute function with error handling
safe_execute <- function(func, ..., error_message = "Operation failed") {
  tryCatch({
    result <- func(...)
    return(result)
  }, error = function(e) {
    message(paste(error_message, ":", e$message))
    return(NULL)
  })
}

# Retry function with exponential backoff
retry_with_backoff <- function(func, max_attempts = 3, base_delay = 1, ...) {
  for (attempt in 1:max_attempts) {
    result <- tryCatch({
      func(...)
    }, error = function(e) {
      if (attempt == max_attempts) {
        stop("Failed after ", max_attempts, " attempts: ", e$message)
      } else {
        message("Attempt ", attempt, " failed: ", e$message, ". Retrying...")
        Sys.sleep(base_delay * (2 ^ (attempt - 1)))  # Exponential backoff
        return("RETRY")
      }
    })
    
    if (!identical(result, "RETRY")) {
      return(result)
    }
  }
}

# ======================================================================
# PROGRESS TRACKING HELPERS
# ======================================================================

# Simple progress tracker
create_progress_tracker <- function(total_items, description = "Processing") {
  start_time <- Sys.time()
  
  update_progress <- function(current_item, item_name = NULL) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    percent <- round((current_item / total_items) * 100, 1)
    eta <- if (current_item > 0) elapsed * (total_items - current_item) / current_item else NA
    
    status <- paste0(description, ": ", current_item, "/", total_items, 
                     " (", percent, "%)")
    
    if (!is.null(item_name)) {
      status <- paste0(status, " - ", item_name)
    }
    
    if (!is.na(eta)) {
      eta_formatted <- if (eta > 60) paste0(round(eta/60, 1), "m") else paste0(round(eta, 0), "s")
      status <- paste0(status, " - ETA: ", eta_formatted)
    }
    
    cat("\r", status, sep = "")
    if (current_item == total_items) cat("\n")
  }
  
  return(update_progress)
}

# ======================================================================
# DATA SUMMARY HELPERS
# ======================================================================

# Create summary statistics for numeric data
summarize_numeric <- function(data, column_name) {
  if (!is_valid_data(data) || !column_name %in% colnames(data)) {
    return(NULL)
  }
  
  values <- data[[column_name]]
  if (!is.numeric(values)) return(NULL)
  
  summary_stats <- list(
    count = length(values),
    missing = sum(is.na(values)),
    mean = mean(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE),
    max = max(values, na.rm = TRUE),
    sd = sd(values, na.rm = TRUE)
  )
  
  return(summary_stats)
}

# Create data quality report
create_data_quality_report <- function(data, dataset_name = "Dataset") {
  if (!is_valid_data(data)) {
    return(paste("❌", dataset_name, "- Invalid or empty data"))
  }
  
  report <- list(
    dataset_name = dataset_name,
    total_rows = nrow(data),
    total_columns = ncol(data),
    column_names = colnames(data),
    missing_data = sapply(data, function(x) sum(is.na(x))),
    data_types = sapply(data, class)
  )
  
  # Calculate completeness percentage
  total_cells <- nrow(data) * ncol(data)
  missing_cells <- sum(report$missing_data)
  report$completeness_percent <- round(((total_cells - missing_cells) / total_cells) * 100, 2)
  
  return(report)
}

# ======================================================================
# PACKAGE MANAGEMENT HELPERS
# ======================================================================

# Check and install required packages
check_required_packages <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages)
  }
  
  # Load all packages
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      stop("Failed to load package: ", pkg)
    }
  }
  
  cat("✓ All required packages loaded\n")
}

# ======================================================================
# INFORMATION DISPLAY HELPERS
# ======================================================================

# Print a formatted header
print_header <- function(text, char = "=", width = 60) {
  border <- paste(rep(char, width), collapse = "")
  cat("\n", border, "\n", sep = "")
  cat(" ", text, "\n")
  cat(border, "\n\n", sep = "")
}

# Print system information
print_system_info <- function() {
  print_header("SYSTEM INFORMATION")
  
  cat("R Version:", R.version.string, "\n")
  cat("Platform:", R.version$platform, "\n")
  cat("Operating System:", Sys.info()["sysname"], "\n")
  cat("Working Directory:", getwd(), "\n")
  cat("Available Memory:", format(object.size(ls(envir = .GlobalEnv)), units = "MB"), "\n")
  
  # API Keys Status
  keys <- check_api_keys()
  cat("\nAPI Keys Status:\n")
  cat("  Census API:", ifelse(keys$census, "✓ Available", "✗ Missing"), "\n")
  cat("  OpenWeather API:", ifelse(keys$openweather, "✓ Available", "✗ Missing"), "\n")
  
  cat("\n")
}

# ======================================================================
# EXPORT MESSAGE
# ======================================================================

cat("✅ Helper functions loaded successfully!\n")
cat("📚 Available functions:\n")
cat("  • API Management: setup_all_api_keys(), check_api_keys(), create_renviron_template()\n")
cat("  • Data Validation: is_valid_data(), safe_extract(), clean_city_name()\n") 
cat("  • File Management: ensure_directory(), safe_save()\n")
cat("  • Error Handling: safe_execute(), retry_with_backoff()\n")
cat("  • Progress Tracking: create_progress_tracker()\n")
cat("  • Data Summary: summarize_numeric(), create_data_quality_report()\n")
cat("  • System Info: print_header(), print_system_info()\n")