# Contributing to Climate Safe Cities

## How to Contribute

1. **Fork the repository**
2. **Create a feature branch** (`git checkout -b feature/amazing-feature`)
3. **Commit your changes** (`git commit -m 'Add amazing feature'`)
4. **Push to the branch** (`git push origin feature/amazing-feature`)
5. **Open a Pull Request**

## Development Setup

1. Clone your fork
2. Install dependencies: `source("01_setup/01_install_packages.R")`
3. Set up API keys (see README)
4. Run tests: `source("03_data_processing/01_data_quality_checks.R")`

## Adding New Cities

To add a new city to the analysis:

1. Add city details to `02_data_collection/04_city_database.R`
2. Ensure proper latitude/longitude coordinates
3. Test data collection for the new city
4. Update documentation

## Code Style

- Use tidyverse conventions
- Comment complex functions
- Include error handling
- Write descriptive commit messages

## Reporting Issues

Please use GitHub Issues to report bugs or request features.