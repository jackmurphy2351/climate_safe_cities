# Create modular project structure for Climate Safe Cities
create_project_structure <- function() {
  
  # Create main directories
  dirs <- c(
    "01_setup",
    "02_data_collection", 
    "03_data_processing",
    "04_visualization",
    "05_shiny_app",
    "05_shiny_app/modules",
    "99_utilities",
    "data",
    "data/raw",
    "data/processed", 
    "data/chunks",
    "outputs",
    "outputs/plots",
    "outputs/reports",
    "docs"
  )
  
  for (dir in dirs) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    cat("✓ Created:", dir, "\n")
  }
  
  cat("\n🎉 Project structure created successfully!\n")
}

# Run this once to set up your project
create_project_structure()
