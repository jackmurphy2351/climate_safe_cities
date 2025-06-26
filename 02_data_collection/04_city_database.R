# ======================================================================
# GLOBAL CITIES DATABASE (100+ Cities)
# ======================================================================

# Comprehensive global cities list optimized for climate diversity
global_cities_database <- tribble(
  ~city_name, ~country_name, ~country_iso, ~lat, ~lon, ~population_millions, ~climate_zone, ~region,
  
  # NORTH AMERICA
  "New York", "United States", "USA", 40.7128, -74.0060, 8.3, "Humid continental", "North America",
  "Los Angeles", "United States", "USA", 34.0522, -118.2437, 3.9, "Mediterranean", "North America",
  "Chicago", "United States", "USA", 41.8781, -87.6298, 2.7, "Humid continental", "North America",
  "Houston", "United States", "USA", 29.7604, -95.3698, 2.3, "Humid subtropical", "North America",
  "Phoenix", "United States", "USA", 33.4484, -112.0740, 1.7, "Hot desert", "North America",
  "Miami", "United States", "USA", 25.7617, -80.1918, 0.5, "Tropical", "North America",
  "Seattle", "United States", "USA", 47.6062, -122.3321, 0.8, "Oceanic", "North America",
  "Toronto", "Canada", "CAN", 43.6532, -79.3832, 2.9, "Humid continental", "North America",
  "Vancouver", "Canada", "CAN", 49.2827, -123.1207, 0.7, "Oceanic", "North America",
  "Mexico City", "Mexico", "MEX", 19.4326, -99.1332, 9.2, "Subtropical highland", "North America",
  
  # SOUTH AMERICA  
  "São Paulo", "Brazil", "BRA", -23.5505, -46.6333, 12.3, "Humid subtropical", "South America",
  "Rio de Janeiro", "Brazil", "BRA", -22.9068, -43.1729, 6.7, "Tropical savanna", "South America",
  "Buenos Aires", "Argentina", "ARG", -34.6118, -58.3960, 15.0, "Humid subtropical", "South America",
  "Lima", "Peru", "PER", -12.0464, -77.0428, 10.7, "Desert", "South America",
  "Bogotá", "Colombia", "COL", 4.7110, -74.0721, 7.4, "Subtropical highland", "South America",
  "Santiago", "Chile", "CHL", -33.4489, -70.6693, 6.2, "Mediterranean", "South America",
  
  # EUROPE
  "London", "United Kingdom", "GBR", 51.5074, -0.1278, 9.0, "Oceanic", "Europe",
  "Paris", "France", "FRA", 48.8566, 2.3522, 2.1, "Oceanic", "Europe",
  "Berlin", "Germany", "DEU", 52.5200, 13.4050, 3.7, "Humid continental", "Europe",
  "Madrid", "Spain", "ESP", 40.4168, -3.7038, 3.2, "Hot-summer Mediterranean", "Europe",
  "Rome", "Italy", "ITA", 41.9028, 12.4964, 2.9, "Mediterranean", "Europe",
  "Amsterdam", "Netherlands", "NLD", 52.3676, 4.9041, 0.9, "Oceanic", "Europe",
  "Stockholm", "Sweden", "SWE", 59.3293, 18.0686, 1.0, "Humid continental", "Europe",
  "Athens", "Greece", "GRC", 37.9755, 23.7348, 3.2, "Hot-summer Mediterranean", "Europe",
  "Vienna", "Austria", "AUT", 48.2082, 16.3738, 1.9, "Humid continental", "Europe",
  "Warsaw", "Poland", "POL", 52.2297, 21.0122, 1.8, "Humid continental", "Europe",
  
  # ASIA
  "Tokyo", "Japan", "JPN", 35.6762, 139.6503, 13.9, "Humid subtropical", "Asia",
  "Shanghai", "China", "CHN", 31.2304, 121.4737, 24.3, "Humid subtropical", "Asia",
  "Beijing", "China", "CHN", 39.9042, 116.4074, 21.5, "Humid continental", "Asia",
  "Mumbai", "India", "IND", 19.0760, 72.8777, 20.4, "Tropical wet and dry", "Asia",
  "Delhi", "India", "IND", 28.7041, 77.1025, 30.3, "Hot semi-arid", "Asia",
  "Seoul", "South Korea", "KOR", 37.5665, 126.9780, 9.7, "Humid continental", "Asia",
  "Bangkok", "Thailand", "THA", 13.7563, 100.5018, 10.5, "Tropical savanna", "Asia",
  "Jakarta", "Indonesia", "IDN", -6.2088, 106.8456, 10.6, "Tropical rainforest", "Asia",
  "Manila", "Philippines", "PHL", 14.5995, 120.9842, 13.5, "Tropical savanna", "Asia",
  "Singapore", "Singapore", "SGP", 1.3521, 103.8198, 5.9, "Tropical rainforest", "Asia",
  "Kuala Lumpur", "Malaysia", "MYS", 3.1390, 101.6869, 1.8, "Tropical rainforest", "Asia",
  "Ho Chi Minh City", "Vietnam", "VNM", 10.8231, 106.6297, 9.0, "Tropical savanna", "Asia",
  
  # MIDDLE EAST
  "Dubai", "United Arab Emirates", "ARE", 25.2048, 55.2708, 3.4, "Hot desert", "Asia",
  "Riyadh", "Saudi Arabia", "SAU", 24.7136, 46.6753, 7.0, "Hot desert", "Asia",
  "Tehran", "Iran", "IRN", 35.6892, 51.3890, 8.7, "Hot semi-arid", "Asia",
  "Istanbul", "Turkey", "TUR", 41.0082, 28.9784, 15.5, "Humid subtropical", "Asia",
  
  # AFRICA
  "Cairo", "Egypt", "EGY", 30.0444, 31.2357, 20.9, "Hot desert", "Africa",
  "Lagos", "Nigeria", "NGA", 6.5244, 3.3792, 15.4, "Tropical savanna", "Africa",
  "Johannesburg", "South Africa", "ZAF", -26.2041, 28.0473, 4.4, "Subtropical highland", "Africa",
  "Cape Town", "South Africa", "ZAF", -33.9249, 18.4241, 4.6, "Mediterranean", "Africa",
  "Nairobi", "Kenya", "KEN", -1.2921, 36.8219, 4.4, "Subtropical highland", "Africa",
  "Casablanca", "Morocco", "MAR", 33.5731, -7.5898, 3.4, "Hot-summer Mediterranean", "Africa",
  "Addis Ababa", "Ethiopia", "ETH", 9.1450, 38.7451, 3.4, "Subtropical highland", "Africa",
  
  # OCEANIA
  "Sydney", "Australia", "AUS", -33.8688, 151.2093, 5.3, "Humid subtropical", "Oceania",
  "Melbourne", "Australia", "AUS", -37.8136, 144.9631, 5.1, "Oceanic", "Oceania",
  "Brisbane", "Australia", "AUS", -27.4698, 153.0251, 2.6, "Humid subtropical", "Oceania",
  "Perth", "Australia", "AUS", -31.9505, 115.8605, 2.1, "Hot-summer Mediterranean", "Oceania",
  "Auckland", "New Zealand", "NZL", -36.8485, 174.7633, 1.7, "Oceanic", "Oceania"
)

# Export database for use in other scripts
cat("✅ City database loaded\n")