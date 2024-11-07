## any inputs that need to be changed from year to year

## 0. defining the inputs

### 0.1. 2023 run
# post_16_school_census_filename <- "1516_2223_ncy.csv" # this needs to be manually downloaded from the table builder. TO DO - need to write down somewhere what exact steps need to be taken in the table builder to get to the right table.

# final_school_period <- "2223"

# max_year <- 2023 # the later year in the final school period

# max_output_year <- max_year + 9


### 0.2. 2024 run
post_16_school_census_filename <- "https://data.london.gov.uk/download/sub-regional-pupil-projections/e91fc3ae-4719-4f57-96b0-d26becfa8575/1516_tolatest_ncy.csv" # this filepath shouldn't need to be changed, as this filepath will automatically upload the most recent data (if not, then the maintainer needs to upload the latest school census information to the datastore). But it is left here so that, if the user wants to, they can recreate an older version of the projections. This can be done by changing this filepath to one of the specific 1516_[years]_ncy.csv datasets available on the datastore page.

final_school_period <- "2324"

max_year <- 2024 # the later year in the final_school_period above

max_output_year <- max_year + 9
