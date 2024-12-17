## the births file is already nearly ready to use, as downloaded. This is a product put together by Marta. 
## this file just takes in that file and reformats it slightly for our use. 


## 0. setting up libraries and functions

library(data.table)

source("scripts/0_a_inputs.R")

source("functions/fn_aggregate_geographies_2.R")

## 1. reading in data

births <- fread("https://data.london.gov.uk/download/modelled-estimates-of-recent-births/9698d0b1-663c-4594-8687-67469ce07e6d/actual_and_predicted_births.csv")

## 2. cleaning the dataset, for our particular requirements

  ### 2.1. narrowing the dataset down only to ITL2 areas

itl_condition <- grep("TL", births$gss_code, ignore.case = TRUE)

births <- births[itl_condition, ]


  ### 2.2. narrowing down only to up to mid-year estimates, for the best alignment with the cutoff date for school year intake date

months <- tstrsplit(x = births$date, split = "-", fixed = TRUE)[2] # extracting month from the date
months <- unlist(months)

midyear_cond <- months == "07" # saving  the logical condition for midyear dates in a separate vector (doing it this way is better for readability...worse for memory...which should I choose?)

births <- births[midyear_cond, ]


  ### 2.3. removing columns we don't need

to_remove <- c("geography", "sex", # because all entries for geography are now ITL221, and all entries for sex are persons
               "interval_lower", "interval_upper") # if we only have intervals for one or two time periods, there's no point having them. Also, because births are just an input into another model, there is nothing we can do with the uncertainty on births alone anyway. 

births <- births[, -..to_remove]

  ### 2.4. filtering out anything past the end year we specified in inputs

years <- tstrsplit(x = births$date, split = "-", fixed = TRUE)[1] 
years <- as.numeric(unlist(years))

years_to_keep <- years <= max_year

births <- births[years_to_keep, ]



## 3. aggregating to region and country


### 5.1. reading in the lookup
itl_region_lookup <- fread("lookups/itl2_region_lookup.csv")

itl_region_lookup <- itl_region_lookup[, c("itl221cd", "itl121cd")]

### 5.2. aggregating to region
births <- births[, -"gss_name"] # have to do this, because otherwise it messes up the aggregation

births_region <- aggregate_geographies_2(
  data = births,
  lookup = itl_region_lookup,
  geog_from_data = "gss_code",
  geog_from_lookup = "itl221cd",
  geog_to_lookup = "itl121cd",
  count_names = c("annual_births")
)

colnames(births_region)[colnames(births_region) == "itl121cd"] <- "itl221cd" # TO DO - I am actually misnaming the column here. What I should do is have one column that describes the type of geography is - ITL221, ITL121, etc - and then another that gives the code. 

### 5.3 aggregating to all England
itl_region_lookup[, country_code := "E92000001"] 

region_country_lookup <- itl_region_lookup[, c("itl121cd", "country_code")]
region_country_lookup <- unique(region_country_lookup) # maybe it would be better to have this as a line within aggregate_geographies_2

births_england <- aggregate_geographies_2(
  data = births_region,
  lookup = region_country_lookup,
  geog_from_data = "itl221cd",
  geog_from_lookup = "itl121cd",
  geog_to_lookup = "country_code",
  count_names = c("annual_births")
)

colnames(births_england)[colnames(births_england) == "country_code"] <- "itl221cd" # TO DO - again, this isn't right. Need two columns for geography


### 5.4 binding them all into the same dataset
colnames(births_region)[colnames(births_region) == "itl221cd"] <- "gss_code"
colnames(births_england)[colnames(births_england) == "itl221cd"] <- "gss_code"


births_all <- rbind(births, births_region, births_england)


  ### 5.5. adding on the geography names. The reason for doing that is here is because the original script only produced ITL level data, which had the names of the ITL. I had to get rid of the names for geographical aggregation, but I want the output after adding the region and country data to look the exact same as before I did this.  

name_lookup <- fread("lookups/itl_code_name_lookup.csv")

setkey(name_lookup, "itl22cd")
setkey(births_all, "gss_code")

births_all <- name_lookup[births_all]

colnames(births_all)[colnames(births_all) == "itl22cd"] <- "gss_code"
colnames(births_all)[colnames(births_all) == "itl22nm"] <- "gss_name"


## 6. writing the dataset

output_filename <- paste0("data/processed_data/births/itl_births_92_to_", substr(max_year, 3, 4), ".csv")

fwrite(x = births_all,
       file = output_filename) 


rm(list = ls())
gc()
gc()
gc()

