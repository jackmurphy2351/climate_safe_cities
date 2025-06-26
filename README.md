# Climate Safe Cities: Global Urban Climate Vulnerability Analysis

[![R](https://img.shields.io/badge/R-4.0%2B-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Data Sources](https://img.shields.io/badge/Data-NASA%20POWER%20%7C%20World%20Bank%20%7C%20US%20Census-green.svg)](#data-sources)

A comprehensive framework for assessing climate vulnerability across major global cities using multiple data sources, advanced analytics, and reproducible R workflows.

## 🌍 Project Overview

This project develops a standardized climate vulnerability index for cities worldwide, combining:
- **Climate Risk Assessment**: Temperature and precipitation extremes
- **Economic Resilience Analysis**: GDP, economic diversity, gender inclusion
- **Social Vulnerability Mapping**: Demographics and adaptive capacity
- **Composite Vulnerability Scoring**: 0-1 scale with categorical rankings

## 🏆 Key Findings

| City | Vulnerability Score | Category | Key Risk Factors |
|------|-------------------|----------|------------------|
| Mumbai | 0.630 | High | Precipitation risk, economic constraints |
| Tokyo | 0.363 | Moderate | Natural disaster exposure |
| Los Angeles | 0.249 | Low | Heat risk, strong economy |
| São Paulo | 0.199 | Low | Balanced risk profile |
| New York | 0.108 | Low | Economic resilience |
| London | 0.030 | Low | Temperate climate, strong adaptive capacity |

### Critical Insights
- **Precipitation risk (r=0.902)** is the strongest vulnerability predictor
- **Economic resilience (r=0.747)** provides significant protection
- **Developing economies** show higher vulnerability despite varying climates

## 📊 Cities Analyzed

- 🇬🇧 **London** (Oceanic climate)
- 🇺🇸 **Los Angeles** (Mediterranean climate)  
- 🇮🇳 **Mumbai** (Tropical monsoon climate)
- 🇺🇸 **New York** (Humid continental climate)
- 🇧🇷 **São Paulo** (Subtropical climate)
- 🇯🇵 **Tokyo** (Humid subtropical climate)

## 🛠️ Technical Architecture

### Data Pipeline
Raw Data Sources → Collection Functions → Quality Checks → Vulnerability Index → Analysis & Visualization

### Technology Stack
- **Language**: R 4.0+
- **Data Sources**: NASA POWER, World Bank API, US Census ACS
- **Key Packages**: `tidyverse`, `nasapower`, `wbstats`, `tidycensus`
- **Architecture**: Modular, memory-optimized for 100+ cities
- **Reproducibility**: Fully documented workflow

## 📁 Repository Structure
climate_safe_cities/
├── 01_setup/                          # Environment configuration
│   ├── 01_install_packages.R          # Package management
│   └── 02_api_keys_setup.R            # API authentication
├── 02_data_collection/                # Data acquisition modules
│   ├── 01_weather_functions.R         # NASA POWER integration
│   ├── 02_worldbank_functions.R       # Economic/social indicators
│   ├── 03_census_functions.R          # US demographic data
│   ├── 04_city_database.R             # Global cities database
│   └── 05_batch_collection.R          # Memory-efficient batch processing
├── 03_data_processing/                # Analysis pipeline
│   ├── 01_data_quality_checks.R       # Automated QA/QC
│   └── 02_vulnerability_index.R       # Core vulnerability calculations
├── 99_utilities/                      # Support functions
│   ├── helper_functions.R             # Utility functions
│   └── memory_management.R            # Large-scale processing tools
├── outputs/                           # Generated results
│   └── vulnerability_analysis/        # Vulnerability index outputs
└── docs/                              # Documentation
└── methodology.md                 # Detailed methodology

## 🚀 Quick Start

### Prerequisites
- R 4.0 or higher
- RStudio (recommended)
- API keys (optional but recommended):
  - [US Census API](https://api.census.gov/data/key_signup.html)
  - [OpenWeatherMap API](https://openweathermap.org/api) (for urban heat island analysis)

### Installation

1. **Clone the repository**
   ```bash
   git clone https://github.com/jackmurphy2351/climate_safe_cities.git
   cd climate_safe_cities

Set up environment
r# Install required packages
source("01_setup/01_install_packages.R")

# Configure API keys (optional)
source("01_setup/02_api_keys_setup.R")

Run analysis
r# Load and run complete vulnerability analysis
source("03_data_processing/02_vulnerability_index.R")

# Execute full workflow
all_data <- load_all_city_data()
vulnerability_results <- calculate_vulnerability_index(all_data)
rankings <- create_vulnerability_summary(vulnerability_results)


📈 Methodology
Vulnerability Index Calculation
Vulnerability = Climate Risk - Adaptive Capacity + 0.5
Climate Risk Components:

Temperature extremes and variability
Precipitation patterns and extremes
Climate trend analysis

Adaptive Capacity Components:

Economic resilience (GDP, diversity, inclusion)
Social vulnerability (demographics, health, education)

See detailed methodology for complete technical specifications.
📊 Data Sources
SourceData TypeCoverageTemporal RangeNASA POWERWeather/ClimateGlobal1981-presentWorld BankEconomic/Social200+ countries1960-presentUS Census ACSDemographicsUS cities2010-present
🔬 Extending the Analysis
Adding New Cities
r# Add city to database
new_city <- tribble(
  ~city_name, ~country_name, ~country_iso, ~lat, ~lon,
  "Paris", "France", "FRA", 48.8566, 2.3522
)

# Collect data
collect_city_climate_vulnerability_enhanced("Paris", 2018:2022)
Scaling to 100+ Cities
The framework supports memory-efficient processing for large-scale analysis:
r# Batch processing with memory management
collect_cities_in_chunks(city_list, chunk_size = 15, years = 2018:2022)

📄 License
This project is licensed under the MIT License - see the LICENSE file for details.

🤝 Contributing
Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.
See CONTRIBUTING.md for contribution guidelines.
📞 Contact
John Murphy

GitHub: @jackmurphy2351
Email: [jackmurphy2351@gmail.com]
LinkedIn: [linkedin.com/in/jackmurphy2351]

🙏 Acknowledgments

NASA POWER team for global meteorological data
World Bank for comprehensive development indicators
US Census Bureau for demographic data
R community for excellent package ecosystem

📚 Citation
If you use this framework in your research, please cite:
bibtex@software{murphy2024climate,
  author = {Murphy, John},
  title = {Climate Safe Cities: Global Urban Climate Vulnerability Analysis},
  url = {https://github.com/jackmurphy2351/climate_safe_cities},
  year = {2024}
}

🌍 Building resilient cities through data-driven climate vulnerability assessment