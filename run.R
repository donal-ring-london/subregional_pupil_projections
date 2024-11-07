# run.R
# This script runs the entire Sub-regional Pupil Projections workflow

message("Starting Sub-regional Pupil Projections workflow...")

# Step 1: Check for required packages
source("init.R")

# Step 2: Set up input parameters
message("Setting up input parameters...")
source("scripts/0_a_inputs.R")

# Step 3: Create necessary directories
message("Creating directories...")
source("scripts/0_b_creating_directories.R")

# Step 4: Run the modelling scripts
message("Running modelling scripts...")
source("scripts/0_c_running_the_modelling_scripts.R")

message("Workflow complete. Check the output directories for results.")
