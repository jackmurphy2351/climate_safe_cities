# Climate Safe Cities: Package Installation and Loading
# Run this first to set up all required packages

# Core packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")

# API packages
if (!require("nasapower")) install.packages("nasapower")
if (!require("wbstats")) install.packages("wbstats")
if (!require("tidycensus")) install.packages("tidycensus")

# Optional packages
if (!require("ROpenWeatherMap")) install.packages("ROpenWeatherMap")
if (!require("remotes")) install.packages("remotes")

# Memory and parallel processing
if (!require("future")) install.packages("future")
if (!require("furrr")) install.packages("furrr")

# Load all packages
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(nasapower)
library(wbstats)
library(tidycensus)

cat("✅ All packages loaded successfully!\n")