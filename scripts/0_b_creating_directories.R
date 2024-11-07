## 0. loading inputs and functions
source("scripts/0_a_inputs.R")

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. folders for the processed data
check_and_create_dir("data")

check_and_create_dir("data/processed_data")

check_and_create_dir("data/processed_data/births")

check_and_create_dir("data/processed_data/combined")

check_and_create_dir("data/processed_data/pupil_numbers")


## 2. folders for the output projections
check_and_create_dir("output_projections")

check_and_create_dir("output_projections/for_tests")

check_and_create_dir("output_projections/for_tests/ets_windows")

check_and_create_dir("output_projections/for_tests/past_average_windows")

check_and_create_dir("output_projections/initial_tenyear")

check_and_create_dir("output_projections/intermediate_outputs")


## 3. folders for the plots
run_years <- paste0(max_year, "_", max_output_year)

check_and_create_dir("plots")

check_and_create_dir(paste0("plots/", run_years))

check_and_create_dir(paste0("plots/", run_years, "/all_years_tenyear_forecast"))

check_and_create_dir(paste0("plots/", run_years, "/cohort_progression_with_pis"))

check_and_create_dir(paste0("plots/", run_years, "/reception_ratio_diag"))

check_and_create_dir(paste0("plots/", run_years, "/reception_tenyear"))

check_and_create_dir(paste0("plots/", run_years, "/year_one_tenyear"))
