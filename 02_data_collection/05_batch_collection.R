# Climate Safe Cities: Memory-Efficient Batch Collection (FIXED VERSION)
# Optimized for MacBook Air 8GB RAM

# Load required modules
source("02_data_collection/01_weather_functions.R")
source("02_data_collection/02_worldbank_functions.R") 
source("02_data_collection/03_census_functions.R")
source("02_data_collection/04_city_database.R")
source("99_utilities/memory_management.R")

# ======================================================================
# FIXED SUMMARY EXTRACTION FUNCTION
# ======================================================================

# Extract key summary statistics instead of keeping full data (FIXED)
extract_city_summary <- function(city_data) {
  if (is.null(city_data)) return(NULL)
  
  summary_data <- list()
  
  # Weather summary (FIXED - removed problematic group_by)
  if (!is.null(city_data$weather) && !is.null(city_data$weather$nasa_power)) {
    weather_summary <- city_data$weather$nasa_power %>%
      summarise(
        city_name = first(city_name, na_rm = TRUE),
        data_points = n(),
        avg_temp_max = mean(temp_max_c, na.rm = TRUE),
        avg_temp_min = mean(temp_min_c, na.rm = TRUE),
        total_precipitation = sum(precipitation_mm, na.rm = TRUE),
        temp_variability = sd(temp_avg_c, na.rm = TRUE),
        precip_variability = sd(precipitation_mm, na.rm = TRUE),
        data_start = min(date, na.rm = TRUE),
        data_end = max(date, na.rm = TRUE),
        .groups = "drop"
      )
    summary_data$weather <- weather_summary
  }
  
  # Climate indicators summary (FIXED)
  if (!is.null(city_data$climate)) {
    # Check which ID column exists
    id_col <- intersect(c("indicator_id", "indicatorID", "id"), colnames(city_data$climate))[1]
    
    if (!is.na(id_col)) {
      climate_summary <- city_data$climate %>%
        group_by(!!sym(id_col)) %>%
        summarise(
          latest_value = last(value, order_by = date),
          avg_value = mean(value, na.rm = TRUE),
          .groups = "drop"
        )
      summary_data$climate <- climate_summary
    } else {
      # Fallback if no ID column found
      climate_summary <- city_data$climate %>%
        summarise(
          indicators_count = n(),
          latest_year = max(date, na.rm = TRUE),
          .groups = "drop"
        )
      summary_data$climate <- climate_summary
    }
  }
  
  # Economic indicators summary (FIXED)
  if (!is.null(city_data$economic)) {
    id_col <- intersect(c("indicator_id", "indicatorID", "id"), colnames(city_data$economic))[1]
    
    if (!is.na(id_col) && "indicator_category" %in% colnames(city_data$economic)) {
      economic_summary <- city_data$economic %>%
        filter(indicator_category == "Economic") %>%
        group_by(!!sym(id_col)) %>%
        summarise(
          latest_value = last(value, order_by = date),
          avg_value = mean(value, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      # Fallback
      economic_summary <- city_data$economic %>%
        summarise(
          indicators_count = n(),
          latest_year = max(date, na.rm = TRUE),
          .groups = "drop"
        )
    }
    summary_data$economic <- economic_summary
  }
  
  return(summary_data)
}

# ======================================================================
# FIXED CITY COLLECTION WITH BETTER ERROR HANDLING
# ======================================================================

# Function to collect data with immediate backup (FIXED)
collect_city_with_backup <- function(city_name, years, save_to_drive = TRUE) {
  
  tryCatch({
    # Collect city data
    city_data <- collect_city_climate_vulnerability_enhanced(city_name, years, use_openweather = FALSE)
    
    if (save_to_drive && !is.null(city_data)) {
      # Save immediately to prevent data loss
      city_filename <- paste0("data/chunks/", clean_city_name(city_name), "_", 
                              min(years), "_", max(years), ".rds")
      ensure_directory("data/chunks")
      saveRDS(city_data, city_filename)
      
      cat("✓ Data saved for", city_name, "\n")
    }
    
    # Return only essential summary to keep in memory
    city_summary <- extract_city_summary(city_data)
    
    # Clean up full data from memory
    rm(city_data)
    gc()  # Force garbage collection
    
    return(city_summary)
    
  }, error = function(e) {
    cat("✗ Error processing", city_name, ":", e$message, "\n")
    return(NULL)
  })
}

# ======================================================================
# IMPROVED CHUNKED PROCESSING WITH VALIDATION
# ======================================================================

# Chunked processing for large city lists (FIXED)
collect_cities_in_chunks <- function(city_list, chunk_size = 25, years = 2018:2022) {
  
  # Validate inputs
  if (length(city_list) == 0) {
    cat("❌ Error: Empty city list provided\n")
    return(list())
  }
  
  # Check which cities are actually in the database
  if (exists("global_cities_database")) {
    available_cities <- intersect(city_list, global_cities_database$city_name)
    missing_cities <- setdiff(city_list, global_cities_database$city_name)
    
    if (length(missing_cities) > 0) {
      cat("⚠️  Warning: The following cities are not in the database:\n")
      cat("   ", paste(missing_cities, collapse = ", "), "\n")
      cat("Available cities from your list:\n")
      cat("   ", paste(available_cities, collapse = ", "), "\n\n")
    }
    
    city_list <- available_cities
  }
  
  if (length(city_list) == 0) {
    cat("❌ Error: No valid cities found in database\n")
    return(list())
  }
  
  # Create chunks
  city_chunks <- split(city_list, ceiling(seq_along(city_list)/chunk_size))
  
  cat("Processing", length(city_list), "cities in", length(city_chunks), "chunks of", chunk_size, "\n")
  
  all_summaries <- list()
  successful_cities <- 0
  
  for (i in seq_along(city_chunks)) {
    cat("\n=== Processing Chunk", i, "of", length(city_chunks), "===\n")
    cat("Cities:", paste(city_chunks[[i]], collapse = ", "), "\n\n")
    
    # Process chunk
    chunk_summaries <- list()
    chunk_success_count <- 0
    
    for (city in city_chunks[[i]]) {
      result <- collect_city_with_backup(city, years, save_to_drive = TRUE)
      
      if (!is.null(result)) {
        chunk_summaries[[city]] <- result
        chunk_success_count <- chunk_success_count + 1
        successful_cities <- successful_cities + 1
      }
    }
    
    # Save chunk summary
    if (length(chunk_summaries) > 0) {
      chunk_filename <- paste0("data/chunks/summary_chunk_", i, ".rds")
      ensure_directory("data/chunks")
      saveRDS(chunk_summaries, chunk_filename)
    }
    
    all_summaries[[i]] <- chunk_summaries
    
    cat("Chunk", i, "complete. Processed", chunk_success_count, "out of", 
        length(city_chunks[[i]]), "cities successfully.\n")
    
    # Memory cleanup between chunks
    force_cleanup(verbose = FALSE)
    
    # Brief pause to prevent API rate limiting
    Sys.sleep(5)
  }
  
  # Combine all summaries
  final_summary <- do.call(c, all_summaries)
  
  cat("\n=== COLLECTION COMPLETE ===\n")
  cat("Successfully processed", successful_cities, "out of", length(city_list), "cities\n")
  
  # Save final summary
  if (length(final_summary) > 0) {
    saveRDS(final_summary, "data/chunks/final_summary.rds")
    cat("✓ Final summary saved to data/chunks/final_summary.rds\n")
  }
  
  return(final_summary)
}

# ======================================================================
# UPDATED CITY DATABASE CHECK
# ======================================================================

# Function to show available cities
show_available_cities <- function() {
  if (exists("global_cities_database")) {
    cat("Cities available in database (", nrow(global_cities_database), " total):\n")
    
    # Group by region for better display
    if ("region" %in% colnames(global_cities_database)) {
      by_region <- global_cities_database %>%
        group_by(region) %>%
        summarise(
          cities = paste(city_name, collapse = ", "),
          count = n(),
          .groups = "drop"
        )
      
      for (i in 1:nrow(by_region)) {
        cat("\n", by_region$region[i], " (", by_region$count[i], " cities):\n")
        cat("  ", by_region$cities[i], "\n")
      }
    } else {
      # Simple list if no region column
      city_names <- global_cities_database$city_name
      for (i in seq(1, length(city_names), by = 5)) {
        end_idx <- min(i + 4, length(city_names))
        cat("  ", paste(city_names[i:end_idx], collapse = ", "), "\n")
      }
    }
  } else {
    cat("❌ global_cities_database not found. Please load 04_city_database.R\n")
  }
}

# ======================================================================
# READY-TO-USE COMMANDS (UPDATED)
# ======================================================================

cat("
=== READY TO COLLECT DATA (FIXED VERSION) ===

# First, see what cities are available:
show_available_cities()

# Start small (test with first 3 cities from database):
test_cities <- head(global_cities_database$city_name, 3)
test_results <- collect_cities_in_chunks(test_cities, chunk_size = 2, years = 2020:2022)

# Medium scale (major cities >5M population):
major_cities <- global_cities_database %>% filter(population_millions > 5) %>% pull(city_name)
major_results <- collect_cities_in_chunks(major_cities, chunk_size = 8, years = 2018:2022)

# Large scale (all cities in database):
all_results <- collect_cities_in_chunks(global_cities_database$city_name, chunk_size = 10, years = 2018:2022)

# Monitor memory throughout:
check_memory_usage()
")

cat("✅ Fixed batch collection functions loaded\n")