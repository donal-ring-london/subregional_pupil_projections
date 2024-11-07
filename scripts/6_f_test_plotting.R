## Plotting full real/central vs bootstrapped results

## 0. libraries and functions
library(data.table)

source("scripts/0_a_inputs.R")



## 1. reading in data

### name lookup
code_name_lookup <- fread("lookups/itl_code_name_lookup.csv")


### all of the bootstrapped datasets
bootstrapped_births_reception <- readRDS(paste0("output_projections/intermediate_outputs/reception_bootstrapped_births_", max_year, "_", max_year + 9, ".rds"))
bootstrapped_births_year_one <- readRDS(paste0("output_projections/intermediate_outputs/year_one_bootstrapped_births_", max_year, "_", max_year + 9, ".rds"))


bootstrapped_ratio_reception <- readRDS(paste0("output_projections/intermediate_outputs/reception_bootstrapped_ratio_", max_year, "_", max_year + 9, ".rds"))
bootstrapped_ratio_year_one <- readRDS(paste0("output_projections/intermediate_outputs/year_one_bootstrapped_ratio_", max_year, "_", max_year + 9, ".rds"))


bootstrapped_births_forecast_reception <- readRDS(paste0("output_projections/intermediate_outputs/reception_bootstrapped_births_forecast_", max_year, "_", max_year + 9, ".rds"))
bootstrapped_births_forecast_year_one <- readRDS(paste0("output_projections/intermediate_outputs/year_one_bootstrapped_births_forecast_", max_year, "_", max_year + 9, ".rds"))


bootstrapped_ratio_forecast_reception <- readRDS(paste0("output_projections/intermediate_outputs/reception_bootstrapped_ratio_forecast_", max_year, "_", max_year + 9, ".rds"))
bootstrapped_ratio_forecast_year_one <- readRDS(paste0("output_projections/intermediate_outputs/year_one_bootstrapped_ratio_forecast_", max_year, "_", max_year + 9, ".rds"))


### births
births_filename <- paste0("data/processed_data/births/itl_births_92_to_", substr(max_year, 3, 4), ".csv")

births <- fread(births_filename)

births[, year := (tstrsplit(date, "-", fixed = TRUE)[1])]
births[, year := as.numeric(year)]

births[, year_lag_4 := year + 4]

births <- births[year > 2010 & year < max_year, ] # NOTE - keeping in 2010 and not updating, meaning the time series for births will get longer for each year. Fine for now, and not going to make much of a difference, but this is something we might like to revisit later.

births[, gss_name := gsub("/", "&", gss_name, fixed = TRUE)] # need to change name of one itl because it has a slash in it


### reception_ratio
last_useful_year <- max_year - 1
births_reception_filename <- paste0("data/processed_data/combined/births_reception_lag4_", last_useful_year, ".csv")

births_reception <- fread(births_reception_filename)

births_reception[, ratio := headcount / annual_births_lag4]


### year_one_ratio



### the central forecasts
central_births_forecast_reception <- readRDS(paste0("output_projections/intermediate_outputs/reception_bootstrapped_births_forecast_", max_year, "_", max_year + 9, ".rds"))
central_births_forecast_year_one <- readRDS(paste0("output_projections/intermediate_outputs/year_one_bootstrapped_births_forecast_", max_year, "_", max_year + 9, ".rds"))


central_ratio_forecast_reception <- readRDS(paste0("output_projections/intermediate_outputs/reception_ratio_central_", max_year, "_", max_year + 9, ".rds"))
central_ratio_forecast_year_one <- readRDS(paste0("output_projections/intermediate_outputs/year_one_ratio_central_", max_year, "_", max_year + 9, ".rds"))


## 2. making the selections for reception ratio and defining the inputs that will be the same for all graphs in this run
all_geogs <- births[, unique(gss_code)]

input_bootstrapped <- bootstrapped_ratio_reception
input_real <- births_reception

output_bootstrapped <- bootstrapped_ratio_forecast_reception
output_central <- central_ratio_forecast_reception

min_input_year <- input_real[, min(year)] # assuming this dataset has a column named "year", which it should
max_input_year <- input_real[, max(year)] # assuming this dataset has a column named "year", which it should

min_output_year <- min(time(output_central[[1]]))
max_output_year <- max(time(output_central[[1]]))

input_boot_col <- rgb(200, 200, 200, alpha = 10, maxColorValue = 255)
output_boot_col <- rgb(0, 255, 0, alpha = 10, maxColorValue = 255)

## 3. plotting (looping through all geographies, making the plot)

### narrowing down on geography, selecting the x and y axis limits

for (i in 1:length(all_geogs)) {
  geog <- all_geogs[i]

  geog_name <- code_name_lookup[itl22cd == geog, itl22nm]

  png(
    file = paste0("plots/", max_year, "_", max_year + 9, "/reception_ratio_diag/", geog_name, ".png"),
    height = 7, width = 12, units = "in", res = 600
  )

  ylim_min <- min(
    input_bootstrapped[[geog]], input_real[itl_cd == geog, ratio],
    output_bootstrapped[[geog]], output_central[[geog]]
  )

  ylim_max <- max(
    input_bootstrapped[[geog]], input_real[itl_cd == geog, ratio],
    output_bootstrapped[[geog]], output_central[[geog]]
  )

  ### setting the plot frame
  plot(
    x = min_input_year:max_output_year, y = rep(1, length(min_input_year:max_output_year)),
    type = "n", las = 1, ylim = c(ylim_min, ylim_max),
    xlab = "", ylab = "",
    main = geog_name
  )

  ### plotting the input bootstraps
  for (j in 1:nrow(input_bootstrapped[[geog]])) {
    lines(
      x = min_input_year:max_input_year, y = input_bootstrapped[[geog]][j, ],
      col = input_boot_col, lwd = 0.1
    )
  }

  ### plotting the input series
  lines(
    x = min_input_year:max_input_year, y = input_real[itl_cd == geog, ratio],
    col = "red", lwd = 2
  )

  ### plotting the output bootstraps
  for (j in 1:nrow(output_bootstrapped[[geog]])) {
    lines(
      x = min_output_year:max_output_year, y = output_bootstrapped[[geog]][j, ],
      col = output_boot_col, lwd = 0.1
    )
  }

  ### plotting the output series
  lines(
    x = min_output_year:max_output_year, y = output_central[[geog]],
    col = "blue", lwd = 2
  )

  dev.off()
}


## BELOW NEEDS TO BE FIXED - copied from the ratio stuff, and then needs to be tailored a bit to fit births

## 4. making the selections for reception ratio and defining the inputs that will be the same for all graphs in this run
all_geogs <- births[, unique(gss_code)]

input_bootstrapped <- bootstrapped_births_reception
input_real <- births

output_bootstrapped <- bootstrapped_births_forecast_reception
output_central <- central_births_forecast_reception

min_input_year <- input_real[, min(year)] # assuming this dataset has a column named "year", which it should
max_input_year <- input_real[, max(year)] # assuming this dataset has a column named "year", which it should

min_output_year <- min(time(output_central[[1]]))
max_output_year <- max(time(output_central[[1]]))

input_boot_col <- rgb(200, 200, 200, alpha = 10, maxColorValue = 255)
output_boot_col <- rgb(0, 255, 0, alpha = 10, maxColorValue = 255)

## 5. plotting (looping through all geographies, making the plot)

### narrowing down on geography, selecting the x and y axis limits
i <- 1
j <- 1

for (i in 1:length(all_geogs)) {
  ### narrowing down on geography, starting the plot, selecting the x and y axis limits

  geog <- all_geogs[i]

  geog_name <- code_name_lookup[itl22cd == geog, itl22nm]

  png(
    file = paste0("plots/", max_year, "_", max_year + 9, "/reception_ratio_diag/", geog_name, ".png"),
    height = 7, width = 12, units = "in", res = 600
  )

  ylim_min <- min(
    input_bootstrapped[[geog]], input_real[itl_cd == geog, ratio],
    output_bootstrapped[[geog]], output_central[[geog]]
  )

  ylim_max <- max(
    input_bootstrapped[[geog]], input_real[itl_cd == geog, ratio],
    output_bootstrapped[[geog]], output_central[[geog]]
  )

  ### setting the plot frame
  plot(
    x = min_input_year:max_output_year, y = rep(1, length(min_input_year:max_output_year)),
    type = "n", las = 1, ylim = c(ylim_min, ylim_max),
    xlab = "", ylab = "",
    main = geog_name
  )

  ### plotting the input bootstraps
  for (j in 1:nrow(input_bootstrapped[[geog]])) {
    lines(
      x = min_input_year:max_input_year, y = input_bootstrapped[[geog]][j, ],
      col = input_boot_col, lwd = 0.1
    )
  }

  ### plotting the input series
  lines(
    x = min_input_year:max_input_year, y = input_real[itl_cd == geog, ratio],
    col = "red", lwd = 2
  )

  ### plotting the output bootstraps
  for (j in 1:nrow(output_bootstrapped[[geog]])) {
    lines(
      x = min_output_year:max_output_year, y = output_bootstrapped[[geog]][j, ],
      col = output_boot_col, lwd = 0.1
    )
  }

  ### plotting the output series
  lines(
    x = min_output_year:max_output_year, y = output_central[[geog]],
    col = "blue", lwd = 2
  )

  dev.off()
}
