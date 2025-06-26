# Climate Safe Cities: Master Control Script
# Run sections as needed

# =============================================================================
# SETUP (Run once)
# =============================================================================

# 1. Install packages
source("01_setup/01_install_packages.R")

# 2. Set up API keys  
source("01_setup/02_api_keys_setup.R")

# 3. Create project directories
# source("01_setup/03_project_directories.R")

# =============================================================================
# DATA COLLECTION (Run as needed)
# =============================================================================

# 4. Load data collection functions
source("02_data_collection/01_weather_functions.R")
source("02_data_collection/02_worldbank_functions.R") 
source("02_data_collection/03_census_functions.R")
source("02_data_collection/04_city_database.R")
source("02_data_collection/05_batch_collection.R")

# 5. Run data collection
# Uncomment the collection level you want:

# Small test (10 cities)
# test_cities <- head(global_cities_database$city_name, 10)
# test_results <- collect_cities_in_chunks(test_cities, chunk_size = 5)

# Medium scale (50 cities)
# major_cities <- global_cities_database %>% filter(population_millions > 5) %>% pull(city_name)
# major_results <- collect_cities_in_chunks(major_cities, chunk_size = 10)

# Large scale (100+ cities)
# all_results <- collect_cities_in_chunks(global_cities_database$city_name, chunk_size = 15)

# =============================================================================
# DATA ANALYSIS
# =============================================================================

# 6. Process and analyze data
source("03_data_processing/01_data_quality_checks.R")
source("03_data_processing/02_worldbank_data_fix.R")
source("03_data_processing/03_vulnerability_index.R")

          # Complete workflow - run each step:
          
          # Step 1: Load all city data
          all_data <- load_all_city_data()
          
          # Step 2: Calculate vulnerability index for all cities
          vulnerability_results <- calculate_vulnerability_index(all_data)
          
          # Step 3: Generate summary and rankings
          rankings <- create_vulnerability_summary(vulnerability_results)
          
          # Step 4: Analyze correlations between factors
          correlations <- analyze_vulnerability_correlations(vulnerability_results)
          
          # Step 5: Save all results
          save_vulnerability_results(vulnerability_results, correlations)


# 7. Create visualizations
# source("04_visualization/01_exploratory_plots.R")

# 8. Run Shiny app
# shiny::runApp("05_shiny_app")

cat("🎯 Ready to run Climate Safe Cities analysis!\n")
cat("📝 Edit this script to uncomment the sections you want to run.\n")