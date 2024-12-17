#### this script is focused on the ratio of pupils carrying over from year 1 to year 2, year 2 to year 3, year 3 to year 4, etc...
#### Because these ratios will be used to forecast all nc years past year group 1. 
#### we will take data on pupil numbers in either predicted year 1, or real years 2-10, and then use historical carryover ratios to simulate progression of this cohort up until year 11. 

#### But first, we need to use the historical carryover ratios to make projections of these ratios. This script first calculates all carryover ratios possible in the pupil numbers dataset, by ITL2 sub-region. 
#### then we forecast each series. So for TLC2, we'll have 10 years or so past data of the proportion of pupils who progress from year 2 to year 3. We then project this time series 10 years into the future. 

## 0. libraries and functions
library(data.table)
library(forecast)

source("scripts/0_a_inputs.R")

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in data, small cleaning tasks
pupils_input_filename <- paste0("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_", final_school_period, ".csv")

pupils <- fread(pupils_input_filename)

pupils[, year := as.numeric(substr(time_period, 1, 4))] ## creating the year variable

pupils <- pupils[, c("year", "itl221cd", "nc_year", "headcount")] ## narrowing down to columns we want

## 2. adding previous year onto current (as in, matching this year's year 1 with last year's reception, this year's year 2 with last year's year 1, etc)
## doing this by first creating an alternate dataset, with the same information aside from a different year variable. This year variable is lagged by one year. 
## There is also a new variable added to this dataset. This is next year's nc year. So for year 3, the corresponding entry in this new variable is year 4, as an example. 
## then we join the two datasets, in such a way as we end up with (for example), year 1 in 2011, year 2 in 2012, year 3 in 2013, etc...

  ### 2.1. creating duplicate dataset, containing last year's headcount for last year's students
pupils_lagged <- data.table(
  year_lag1 = pupils[, year] + 1,
  itl221cd = pupils[, itl221cd],
  nc_year = pupils[, nc_year],
  previous_headcount = pupils[, headcount]
)

prev_ncyear_lookup <- data.table(
  current_nc_year = c("reception" ,"year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", 
                       "year_group_7", "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13"), 
  next_nc_year = c("year_group_1" ,"year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                      "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
)

setkey(pupils_lagged, "nc_year")
setkey(prev_ncyear_lookup, "current_nc_year")

pupils_lagged <- prev_ncyear_lookup[pupils_lagged]

pupils_lagged <- pupils_lagged[, c("year_lag1", "itl221cd", "next_nc_year", "previous_headcount")]


  ### 2.2. joining the two datasets and calculating the proportion carrying over
pupils <- pupils_lagged[pupils, on = c(year_lag1 = "year", itl221cd = "itl221cd", next_nc_year = "nc_year")]

pupils <- pupils[, c("year_lag1", "itl221cd", "next_nc_year", "headcount", "previous_headcount")] # selecting and rearranging columns

pupils[, proportion_continuing := headcount/previous_headcount]

pupils <- pupils[!is.na(proportion_continuing),] # removing na values, that are generated from nc years where there is no previous nc year. 



## 3. quick cleaning, writing the final dataset as a standalone output, before doing any projections. 

colnames(pupils)[colnames(pupils) == "next_nc_year"] <- "nc_year"
colnames(pupils)[colnames(pupils) == "itl221cd"] <- "itl22cd"
colnames(pupils)[colnames(pupils) == "year_lag1"] <- "year"

output_ratio_filename <- paste0("data/processed_data/pupil_numbers/pupils_ratio_carrying_over_", final_school_period, ".csv")

fwrite(x = pupils,
       file = output_ratio_filename)


## 4. projecting forwards the ratios for each of the years

pupils <- pupils[!(nc_year %in% c("year_group_12", "year_group_13", "year_group_14")), ] # assuming for now that we're not modelling past year 11. Might need to be revised later. 

  ### 4.1. getting the ratio series for each geography for each nc_year into a time series object. 
  ### using a for loop within a for loop here. It's typically not recommended to do this, but I think this is more readable than any alternatives (such as a complicated structure of lapplys, or an lapply in a loop) and there isn't a meaningful performance punishment. 

nc_years <- c("year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", 
              "year_group_7", "year_group_8", "year_group_9", "year_group_10", "year_group_11")

geogs <- pupils[, unique(itl22cd)]

all_ratios_ts_list <- list()

help(par)

for(j in 1:length(nc_years)){ # first for loop - choosing an nc year, filtering the dataset to this nc year

  nc_year_j <- nc_years[j]
  
  nc_year_data <- pupils[nc_year == nc_year_j, c("year", "itl22cd", "proportion_continuing")]

  nc_year_ratio_tslist <- list()

  for(i in 1:length(geogs)){ # then, for this nc year, looping through the geographies and making a time series. 
                             # Meaning we will end up with a two-level list. For each nc year, a list with each entry containing the historical series for a geography. 
                             # And then all of the nc years are in one big list. 
    
    geog <- geogs[i]
  
    geog_ts <- ts(data = nc_year_data[itl22cd == geog, proportion_continuing],
                  start = min(nc_year_data[itl22cd == geog, year]),
                  frequency = 1)
  
    nc_year_ratio_tslist[[i]] <- geog_ts
  
  }

  names(nc_year_ratio_tslist) <- geogs

  all_ratios_ts_list[[j]] <- nc_year_ratio_tslist
  
}

names(all_ratios_ts_list) <- nc_years


  ### 4.2. projecting forward the ratios
all_nc_years_projected_ratios <- list()

for(j in 1:length(nc_years)){
  
  nc_year_tslist <- all_ratios_ts_list[[j]]
  
  projected_nc_year_ratio <- lapply(
    X = nc_year_tslist,
    FUN = project_with_ets,
    periods_ahead = 10,
    model = "MMN", # MNN if we're are telling it to project no trend. So we're just holding this ratio series constant. 
    damped = TRUE, 
    phi = 0.85
    )
  
  all_nc_years_projected_ratios[[j]] <- projected_nc_year_ratio
  
}

names(all_nc_years_projected_ratios) <- nc_years


## 5. calculating uncertainty for the projections above. We're not interesting in producing predictions for the ratios themselves, but instead for using the uncertainty here as an input into the uncertainty for the final projection, when we multiply these ratios by the starting cohort.
## when the usual method of calculating uncertainty was used - resampling the input data, repeating the projection on each of these resamples - we ended up with very asymmetric distributions of bootstrapped projections around the central forecast. And given that a starting cohort would be multiplied by these asymmetric distributions again and again, the end resulting prediction intervals were very skewed. 
## so now, what we are doing is taking the prediction intervals that are produced automatically by the forecast.ets method, and producing 1,000 bootstrapped samples around these. This way, we have produced a set of bootstrapped projections that are perfectly symmetrically distributed around the central forecast, but that is grounded in a real uncertainty calculation. 
## We are assuming a normal distribution around the estimate, which is probably not a sound assumption, but I think it's good enough for this case. 
## UPDATE - no, not what we're doing now. Calculating them in the normal bootstrapping way, and then just adjusting the intervals so that they are symmetric. 

  ### 5.1. extract the residuals

all_nc_years_resids <- list()

for(nc_year_sel in nc_years){
  
  resids <- lapply(
    X = all_ratios_ts_list[[nc_year_sel]], 
    FUN = extract_residuals_ets,
    model = "MMN",
    damped = TRUE,
    phi = 0.85
  )
  
  all_nc_years_resids[[nc_year_sel]] <- resids
  
}


  ### 5.2. resample the residuals
all_nc_years_resid_resamples <- list()

for(nc_year_sel in nc_years){
  
  resid_resamples <- lapply(
    X = all_nc_years_resids[[nc_year_sel]],
    FUN = resample_for_bootstrapping,
    n_resamples = 1000
  )
  
  all_nc_years_resid_resamples[[nc_year_sel]] <- resid_resamples
  
}


  ### 5.3. add the resampled residuals to the original series
  ### double for-loop again...clunky but I still think the most readable way to do this
all_nc_years_final_resamples <- list()

for(nc_year_sel in nc_years){
  
  resid_resamples_nc <- all_nc_years_resid_resamples[[nc_year_sel]]
  
  ratio_ts_nc <- all_ratios_ts_list[[nc_year_sel]]
  
  final_ratio_resamples <- list()
  
  for(geog_sel in geogs){
    
    num_ratio_vec <- as.numeric(ratio_ts_nc[[geog_sel]])
    resamples <- resid_resamples_nc[[geog_sel]]
    
    final_ratio_resamples[[geog_sel]] <- data.table(sweep(resamples, 2, num_ratio_vec, "+"))
    
  }
  
  all_nc_years_final_resamples[[nc_year_sel]] <- final_ratio_resamples
  
}


  ### 5.4. forecasting each resample to get the full bootstrapped uncertainty distribution

first_year <- min(time(all_ratios_ts_list[[1]][[1]]))

all_nc_years_bootstrapped_forecasts <- list()

for(nc_year_sel in nc_years){
  
  resamples_sel <- all_nc_years_final_resamples[[nc_year_sel]]
  
  nc_year_bootstrapped_forecast <- list()
  
  for(geog_sel in geogs){
    
    resamples_geog <- resamples_sel[[geog]]  
    
    resamples_geog_ts <- apply(X = resamples_geog, MARGIN = 1, FUN = ts, 
                               start = first_year, frequency = 1, simplify = FALSE)
    
    resamples_geog_forecast <- lapply(
      X = resamples_geog_ts,
      FUN = project_with_ets,
      periods_ahead = 10,
      model = "MMN",
      damped = TRUE,
      phi = 0.85
    )
    
    resamples_geog_forecast_dt <- convert_tslist_to_dt(resamples_geog_forecast)
    
    nc_year_bootstrapped_forecast[[geog_sel]] <- resamples_geog_forecast_dt
    
  }
  
  all_nc_years_bootstrapped_forecasts[[nc_year_sel]] <- nc_year_bootstrapped_forecast
  
}


bootstrapped_all <- all_nc_years_bootstrapped_forecasts


## 6. getting the projections into one data.table

projected_ratios_dt_list <- list()

for(j in 1:length(all_nc_years_projected_ratios)){
  
  nc_year_projections_dt <- lapply(
    X = all_nc_years_projected_ratios[[j]],
    FUN = function(input_ts){return(data.table(year = time(input_ts), projected_proportion_continuing = input_ts))}
  )
  
  nc_year_projections_dt <- rbindlist(nc_year_projections_dt, idcol = "itl22cd")
  
  projected_ratios_dt_list[[j]] <- nc_year_projections_dt
  
}

names(projected_ratios_dt_list) <- nc_years

projected_ratios_dt <- rbindlist(projected_ratios_dt_list, idcol = "nc_year")


## 7. writing the final output

projected_ratios_dt <- projected_ratios_dt[order(nc_year, itl22cd, year), ]
projected_ratios_dt <- projected_ratios_dt[, c("year", "itl22cd", "nc_year", "projected_proportion_continuing")]

projections_output_filename <- paste0("data/processed_data/pupil_numbers/pupils_projected_ratio_carrying_over_", final_school_period, ".csv")

fwrite(x = projected_ratios_dt,
       file = projections_output_filename)

bootstrapped_projections_filename <- paste0("output_projections/intermediate_outputs/full_bootstrapped_ratios_carryover_", final_school_period, ".RDS")

saveRDS(object = bootstrapped_all,
        file = bootstrapped_projections_filename)


rm(list = ls())
gc()
gc()
gc()
