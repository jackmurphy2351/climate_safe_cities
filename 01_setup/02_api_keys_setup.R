# Climate Safe Cities: API Keys Configuration
# Sets up all required API keys for data collection

# Load helper functions
source("99_utilities/helper_functions.R")

# ======================================================================
# API KEYS SETUP
# ======================================================================

# Print system information
print_system_info()

# Check if .Renviron file exists, create template if not
print_header("CHECKING .RENVIRON FILE")
create_renviron_template()

# Set up all API keys
print_header("CONFIGURING API KEYS")
setup_all_api_keys()

# Final status check
print_header("API SETUP SUMMARY")
keys_status <- check_api_keys()

if (keys_status$census) {
  cat("✅ Census API: Ready for US cities data collection\n")
} else {
  cat("⚠️  Census API: Not configured - US social vulnerability data will be limited\n")
  cat("   Get your key at: https://api.census.gov/data/key_signup.html\n")
}

if (keys_status$openweather) {
  cat("✅ OpenWeatherMap API: Ready for urban heat island analysis\n")
} else {
  cat("⚠️  OpenWeatherMap API: Not configured - will use NASA POWER only\n")
  cat("   Get your key at: https://openweathermap.org/api\n")
}

# NASA POWER doesn't require an API key
cat("✅ NASA POWER: Ready (no API key required)\n")

# World Bank doesn't require an API key  
cat("✅ World Bank API: Ready (no API key required)\n")

cat("\n🎯 NEXT STEPS:\n")
if (!keys_status$census || !keys_status$openweather) {
  cat("1. Edit .Renviron file to add missing API keys\n")
  cat("2. Restart R session to load new keys\n")
  cat("3. Re-run this script to verify setup\n")
} else {
  cat("1. All API keys configured successfully!\n")
  cat("2. Ready to proceed with data collection\n")
  cat("3. Run: source('02_data_collection/05_batch_collection.R')\n")
}

cat("\n" , rep("=", 60), "\n", sep = "")