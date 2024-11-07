## 0. functions and libraries
source("scripts/0_a_inputs.R")

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

library(data.table)

## 1. reading in data
all_projections_to_read <- paste0("output_projections/initial_tenyear/reception_year_11_projections_", max_year, "_", max_output_year, ".csv")

all_projections <- fread(all_projections_to_read)


## 2. making the plots
geogs <- all_projections[, unique(itl22cd)]

geog <- geogs[1]

for (i in 1:length(geogs)) {
  geog <- geogs[i]

  all_projections

  ## extracting the data
  mean_projection <- extract_cohort_data_2(
    start_cohort_year = max_year,
    start_cohort_nc_year = "reception",
    end_cohort_nc_year = "year_group_11",
    geog = geog,
    pupil_data = all_projections,
    data_variable = "mean_projection",
    max_year = max_output_year
  )

  upper_pi <- extract_cohort_data_2(
    start_cohort_year = max_year,
    start_cohort_nc_year = "reception",
    end_cohort_nc_year = "year_group_11",
    geog = geog,
    pupil_data = all_projections,
    data_variable = "upper_pi",
    max_year = max_output_year
  )

  lower_pi <- extract_cohort_data_2(
    start_cohort_year = max_year,
    start_cohort_nc_year = "reception",
    end_cohort_nc_year = "year_group_11",
    geog = geog,
    pupil_data = all_projections,
    data_variable = "lower_pi",
    max_year = max_output_year
  )

  ## plotting the data

  pi_col <- rgb(255, 0, 0, alpha = 100, maxColorValue = 255)

  ylim_min <- min(cbind(mean_projection[, 3], upper_pi[, 3], lower_pi[, 3]))
  ylim_max <- max(cbind(mean_projection[, 3], upper_pi[, 3], lower_pi[, 3]))

  filename <- paste0("plots/", max_year, "_", max_output_year, "/cohort_progression_with_pis/", geog, ".png")

  png(
    file = filename,
    height = 7, width = 14, units = "in", res = 600
  )

  plot(
    x = max_year:max_output_year, y = rep(1, 10), type = "n", las = 1, bty = "n",
    ylim = c(ylim_min, ylim_max)
  )

  polygon(x = c(max_year:max_output_year, rev(max_year:max_output_year)), y = c(upper_pi[, output_data], rev(lower_pi[, output_data])), col = pi_col, border = NA)

  lines(x = max_year:max_output_year, y = mean_projection[, output_data], lwd = 3, col = "blue")

  title(main = geog)

  dev.off()
}
