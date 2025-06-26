# Climate Safe Cities: Global Urban Climate Vulnerability Analysis

A comprehensive analysis of climate vulnerability across major global cities using NASA POWER weather data, World Bank indicators, and social vulnerability metrics.

## Overview
This project develops a climate vulnerability index for cities worldwide, combining:
- Temperature and precipitation risk indicators
- Economic resilience measures  
- Social vulnerability assessments

## Cities Analyzed
- London, UK
- Los Angeles, USA
- Mumbai, India
- New York, USA
- São Paulo, Brazil
- Tokyo, Japan

## Key Findings
Mumbai shows highest vulnerability (0.630), while London shows lowest (0.030). Precipitation risk and economic resilience are the strongest vulnerability drivers.

## Data Sources
- **Weather Data**: NASA POWER
- **Economic/Social Data**: World Bank
- **US Social Vulnerability**: American Community Survey

## Repository Structure
- `01_setup/` - Package installation and API configuration
- `02_data_collection/` - Data collection functions and scripts
- `03_data_processing/` - Data quality checks and vulnerability index
- `99_utilities/` - Helper functions and memory management
- `outputs/` - Analysis results and visualizations

## Requirements
- R 4.0+
- API keys for NOAA and US Census (optional)
- Required packages: tidyverse, nasapower, wbstats, tidycensus

## Usage
1. Set up API keys in `.Renviron`
2. Run data collection: `source("02_data_collection/05_batch_collection.R")`
3. Calculate vulnerability index: `source("03_data_processing/02_vulnerability_index.R")`

## Results
Complete vulnerability analysis results available in `outputs/vulnerability_analysis/`