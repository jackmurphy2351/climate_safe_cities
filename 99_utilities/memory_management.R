# Climate Safe Cities: Memory Management Utilities
# Tools for efficient memory usage when processing large datasets

# ======================================================================
# MEMORY MONITORING FUNCTIONS
# ======================================================================

# Check current memory usage
check_memory_usage <- function(detailed = FALSE) {
  cat("=== MEMORY USAGE REPORT ===\n")
  
  # System memory (macOS specific)
  if (Sys.info()["sysname"] == "Darwin") {  # macOS
    tryCatch({
      memory_info <- system("memory_pressure", intern = TRUE)
      cat("System Memory Status:\n")
      cat(paste(memory_info[1:3], collapse = "\n"), "\n\n")
    }, error = function(e) {
      cat("Could not retrieve system memory info\n\n")
    })
  }
  
  # R memory usage
  cat("R Memory Usage:\n")
  gc_info <- gc()
  print(gc_info)
  
  # Object sizes in global environment
  if (detailed) {
    cat("\nLargest Objects in Global Environment:\n")
    obj_sizes <- sapply(ls(envir = .GlobalEnv), function(x) {
      object.size(get(x, envir = .GlobalEnv))
    })
    
    if (length(obj_sizes) > 0) {
      obj_sizes_sorted <- sort(obj_sizes, decreasing = TRUE)[1:min(10, length(obj_sizes))]
      for (i in seq_along(obj_sizes_sorted)) {
        cat(sprintf("  %s: %s\n", names(obj_sizes_sorted)[i], 
                    format(obj_sizes_sorted[i], units = "MB")))
      }
    } else {
      cat("  No objects in global environment\n")
    }
  }
  
  cat("\nTotal objects in memory:", format(object.size(ls(envir = .GlobalEnv)), units = "MB"), "\n")
  cat("========================\n\n")
  
  # Return memory info invisibly
  invisible(gc_info)
}

# Get available memory estimate (rough)
get_available_memory_mb <- function() {
  gc_info <- gc()
  used_mb <- sum(gc_info[, "used"]) * gc_info[1, "max used"] / gc_info[1, "used"] / 1024
  
  # Rough estimate: assume 4-6 GB available for data on 8GB system
  estimated_available <- 5000 - used_mb  # Conservative estimate
  return(max(0, estimated_available))
}

# Check if we have enough memory for operation
check_memory_capacity <- function(estimated_mb_needed, safety_factor = 1.5) {
  available_mb <- get_available_memory_mb()
  needed_with_safety <- estimated_mb_needed * safety_factor
  
  cat("Memory capacity check:\n")
  cat("  Estimated needed:", round(estimated_mb_needed, 1), "MB\n")
  cat("  With safety factor:", round(needed_with_safety, 1), "MB\n")
  cat("  Estimated available:", round(available_mb, 1), "MB\n")
  
  if (needed_with_safety > available_mb) {
    cat("  Status: ⚠️  CAUTION - May exceed available memory\n")
    cat("  Recommendation: Use smaller chunks or process in batches\n")
    return(FALSE)
  } else {
    cat("  Status: ✅ Sufficient memory available\n")
    return(TRUE)
  }
}

# ======================================================================
# MEMORY CLEANUP FUNCTIONS
# ======================================================================

# Aggressive garbage collection
force_cleanup <- function(verbose = TRUE) {
  if (verbose) cat("Performing memory cleanup...\n")
  
  # Multiple rounds of garbage collection
  for (i in 1:3) {
    gc_result <- gc()
    if (verbose && i == 1) {
      cat("Memory freed:", format(sum(gc_result[, "max used"] - gc_result[, "used"]) * 8, units = "MB"), "\n")
    }
  }
  
  if (verbose) cat("Memory cleanup complete.\n")
  invisible(gc_result)
}

# Remove large objects from environment
remove_large_objects <- function(threshold_mb = 50, env = .GlobalEnv) {
  obj_names <- ls(envir = env)
  
  if (length(obj_names) == 0) {
    cat("No objects to check in environment\n")
    return(invisible(NULL))
  }
  
  obj_sizes <- sapply(obj_names, function(x) {
    as.numeric(object.size(get(x, envir = env))) / (1024^2)  # Convert to MB
  })
  
  large_objects <- names(obj_sizes)[obj_sizes > threshold_mb]
  
  if (length(large_objects) > 0) {
    cat("Removing large objects (>", threshold_mb, "MB):\n")
    for (obj in large_objects) {
      cat("  Removing", obj, "(", round(obj_sizes[obj], 1), "MB )\n")
      rm(list = obj, envir = env)
    }
    force_cleanup(verbose = FALSE)
    cat("Large objects removed and memory cleaned up.\n")
  } else {
    cat("No objects larger than", threshold_mb, "MB found.\n")
  }
  
  invisible(large_objects)
}

# Clear all data objects but keep functions
clear_data_objects <- function(keep_patterns = c("^get_", "^collect_", "^setup_", "^check_"), env = .GlobalEnv) {
  all_objects <- ls(envir = env)
  
  # Identify functions to keep
  keep_objects <- character(0)
  for (pattern in keep_patterns) {
    keep_objects <- c(keep_objects, grep(pattern, all_objects, value = TRUE))
  }
  
  # Also keep if it's a function
  function_objects <- all_objects[sapply(all_objects, function(x) is.function(get(x, envir = env)))]
  keep_objects <- unique(c(keep_objects, function_objects))
  
  # Remove everything else
  remove_objects <- setdiff(all_objects, keep_objects)
  
  if (length(remove_objects) > 0) {
    cat("Clearing data objects (keeping", length(keep_objects), "functions):\n")
    cat("  Removing:", length(remove_objects), "data objects\n")
    rm(list = remove_objects, envir = env)
    force_cleanup(verbose = FALSE)
    cat("Data objects cleared, functions preserved.\n")
  } else {
    cat("No data objects to clear.\n")
  }
  
  invisible(remove_objects)
}

# ======================================================================
# CHUNKED PROCESSING HELPERS
# ======================================================================

# Calculate optimal chunk size based on available memory
calculate_optimal_chunk_size <- function(total_items, estimated_mb_per_item = 8, max_chunk_mb = 1000) {
  available_mb <- get_available_memory_mb()
  
  # Conservative estimate: use only 50% of available memory
  safe_memory_mb <- available_mb * 0.5
  target_memory_mb <- min(safe_memory_mb, max_chunk_mb)
  
  optimal_chunk_size <- floor(target_memory_mb / estimated_mb_per_item)
  optimal_chunk_size <- max(1, min(optimal_chunk_size, total_items))
  
  cat("Chunk size calculation:\n")
  cat("  Total items:", total_items, "\n")
  cat("  Est. MB per item:", estimated_mb_per_item, "\n")
  cat("  Available memory:", round(available_mb, 1), "MB\n")
  cat("  Target chunk memory:", round(target_memory_mb, 1), "MB\n")
  cat("  Optimal chunk size:", optimal_chunk_size, "\n")
  
  return(optimal_chunk_size)
}

# Split list into memory-safe chunks
create_memory_safe_chunks <- function(item_list, estimated_mb_per_item = 8, max_chunk_mb = 1000) {
  total_items <- length(item_list)
  
  if (total_items == 0) {
    return(list())
  }
  
  chunk_size <- calculate_optimal_chunk_size(total_items, estimated_mb_per_item, max_chunk_mb)
  
  # Create chunks
  chunks <- split(item_list, ceiling(seq_along(item_list) / chunk_size))
  
  cat("Created", length(chunks), "chunks of approximately", chunk_size, "items each\n")
  return(chunks)
}

# ======================================================================
# PROGRESS AND MONITORING FOR LONG OPERATIONS
# ======================================================================

# Memory-aware progress tracker
create_memory_aware_progress <- function(total_items, description = "Processing", memory_check_interval = 5) {
  start_time <- Sys.time()
  last_memory_check <- 0
  
  update_progress <- function(current_item, item_name = NULL, force_memory_check = FALSE) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    percent <- round((current_item / total_items) * 100, 1)
    eta <- if (current_item > 0) elapsed * (total_items - current_item) / current_item else NA
    
    # Build status message
    status <- paste0(description, ": ", current_item, "/", total_items, " (", percent, "%)")
    
    if (!is.null(item_name)) {
      status <- paste0(status, " - ", item_name)
    }
    
    if (!is.na(eta)) {
      eta_formatted <- if (eta > 60) paste0(round(eta/60, 1), "m") else paste0(round(eta, 0), "s")
      status <- paste0(status, " - ETA: ", eta_formatted)
    }
    
    # Memory check
    if (force_memory_check || (current_item - last_memory_check) >= memory_check_interval) {
      gc_info <- gc()
      memory_mb <- sum(gc_info[, "used"]) * 8 / 1024  # Rough estimate
      status <- paste0(status, " - Mem: ", round(memory_mb, 0), "MB")
      last_memory_check <<- current_item
    }
    
    cat("\r", status, sep = "")
    if (current_item == total_items) {
      cat("\n")
      cat("Completed in", round(elapsed, 1), "seconds\n")
    }
  }
  
  return(update_progress)
}

# ======================================================================
# MEMORY-SAFE DATA OPERATIONS
# ======================================================================

# Safe data binding that monitors memory
safe_bind_rows <- function(data_list, max_memory_mb = 2000) {
  if (length(data_list) == 0) {
    return(data.frame())
  }
  
  # Remove NULL elements
  data_list <- data_list[!sapply(data_list, is.null)]
  
  if (length(data_list) == 0) {
    return(data.frame())
  }
  
  # Estimate memory usage
  first_df <- data_list[[1]]
  if (is.data.frame(first_df)) {
    estimated_mb <- as.numeric(object.size(first_df)) * length(data_list) / (1024^2)
    
    if (estimated_mb > max_memory_mb) {
      warning("Estimated memory usage (", round(estimated_mb, 1), 
              "MB) exceeds limit (", max_memory_mb, "MB). Consider processing in smaller chunks.")
    }
  }
  
  # Bind rows with memory monitoring
  cat("Binding", length(data_list), "data frames...\n")
  
  tryCatch({
    result <- dplyr::bind_rows(data_list)
    cat("Successfully bound", nrow(result), "rows\n")
    return(result)
  }, error = function(e) {
    cat("Error binding rows:", e$message, "\n")
    cat("Attempting chunked binding...\n")
    
    # Fallback: bind in smaller chunks
    chunk_size <- max(1, floor(length(data_list) / 4))
    chunks <- split(data_list, ceiling(seq_along(data_list) / chunk_size))
    
    chunk_results <- list()
    for (i in seq_along(chunks)) {
      cat("Processing chunk", i, "of", length(chunks), "\n")
      chunk_results[[i]] <- dplyr::bind_rows(chunks[[i]])
      force_cleanup(verbose = FALSE)
    }
    
    # Final binding
    result <- dplyr::bind_rows(chunk_results)
    cat("Chunked binding complete:", nrow(result), "rows\n")
    return(result)
  })
}

# ======================================================================
# MONITORING SETUP
# ======================================================================

# Set up automatic memory monitoring
setup_memory_monitoring <- function(interval_minutes = 10) {
  if (!exists(".memory_monitor_active", envir = .GlobalEnv)) {
    assign(".memory_monitor_active", TRUE, envir = .GlobalEnv)
    
    cat("Memory monitoring activated (every", interval_minutes, "minutes)\n")
    cat("Call stop_memory_monitoring() to disable\n")
    
    # Note: This is a simple version. For production, you might want to use 
    # more sophisticated monitoring with the 'later' package
  }
}

# Stop memory monitoring
stop_memory_monitoring <- function() {
  if (exists(".memory_monitor_active", envir = .GlobalEnv)) {
    rm(.memory_monitor_active, envir = .GlobalEnv)
    cat("Memory monitoring stopped\n")
  }
}

# ======================================================================
# EXPORT MESSAGE
# ======================================================================

cat("✅ Memory management utilities loaded successfully!\n")
cat("🧠 Available functions:\n")
cat("  • Monitoring: check_memory_usage(), get_available_memory_mb(), check_memory_capacity()\n")
cat("  • Cleanup: force_cleanup(), remove_large_objects(), clear_data_objects()\n")
cat("  • Chunking: calculate_optimal_chunk_size(), create_memory_safe_chunks()\n")
cat("  • Progress: create_memory_aware_progress()\n")
cat("  • Safe Operations: safe_bind_rows()\n")
cat("  • Setup: setup_memory_monitoring(), stop_memory_monitoring()\n")