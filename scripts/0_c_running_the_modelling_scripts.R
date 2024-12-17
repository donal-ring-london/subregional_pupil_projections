## a script that runs all of the scripts needed to produce the projections for one run of a year.


## 1. running the data processing and modelling scripts
1+1
start_time <- Sys.time()

source("scripts/1_reformatting_pupil_numbers.R")


source("scripts/2_fitting_pupil_numbers_to_itl.R")


source("scripts/3_a_cleaning_births_data.R")


source("scripts/3_b_combining_births_and_pupils.R")


source("scripts/3_c_creating_ratio_carrying_over_dataset.R")


source("scripts/5_a_reception_10year_projections.R")


source("scripts/5_b_yearone_10year_projections.R")


source("scripts/5_c_primary_projections.R")


source("scripts/5_d_getting_projections_into_one_output.R")

end_model <- Sys.time()


## 2. running the plotting scripts
source("scripts/6_a_plotting_reception.R")


source("scripts/6_b_year_one_plotting.R")


source("scripts/6_c_year_rest_plotting.R")


source("scripts/6_e_cohort_progression_plotting_with_prediction_intervals.R")


end_model_and_plot <- Sys.time()

## start time was about 11.26am

model_time_taken <- end_model - start_time

all_time_taken <- end_model_and_plot - start_time




