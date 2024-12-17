

## 0. setting up libraries and functions

library(data.table)

source("scripts/0_a_inputs.R")

source("functions/fn_convert_geographies_to_latest.R")
source("functions/fn_aggregate_geographies_2.R")


## 1. reading in the la level pupil data

input_filename <- paste0("data/processed_data/pupil_numbers/pupil_numbers_1112_to_", final_school_period, ".csv")

pupil_data <- fread(input_filename)


## 2. getting rid of other la identifiers (because it will mess up the aggregation later. We need only one column that uniquely identifies local authorities)

pupil_data <- pupil_data[, -c("old_la_code", "la_name")]


## 3. fitting all LAs in the dataset to 2021 LA boundaries (in some of the older datasets, e.g. 2011/2012, the local authority codes are older ones, before various mergings and splitting and renamings. This section matches all of those to 2021 codes)

  ### 3.1. reading in the lookup
new_old_la_lookup <- fread("lookups/old_new_las_lookup.csv")


  ### 3.2. aggregating to new LAs
pupil_data <- convert_geographies_to_latest(
  data = pupil_data,
  lookup = new_old_la_lookup,
  geog_from_data = "new_la_code",
  geog_from_lookup = "old_las", 
  geog_to_lookup = "new_las",
  count_names = c("full_time", "part_time", "headcount", "fte")
)

table(pupil_data$new_las)[table(pupil_data$new_las) != 195]



## 4. fitting the local authorities to ITL2 regions

lookup <- fread("lookups/la_itl_lookup.csv")


  ### 4.1. for each local education authority that is coded as a county rather than a local authority (because in the source datasets, the geographies are bit weird and it's a mix of local authorities and some counties), we need to change it to a code that will have a match in the la-itl lookup. 
  ### What we will do is change it to the code corresponding to any of its constituent local authorities. It doesn't matter which one - they will all map onto the correct itl. 

la_county_lookup <- fread("lookups/la_county_lookup.csv")

counties_to_replace <- unique(pupil_data$new_las)[!(unique(pupil_data$new_las) %in% unique(lookup$lad21cd))] # extracting counties in the pupil dataset, which won't have any match in the la-itl lookup

counties_to_replace <- counties_to_replace[counties_to_replace != "E10000021"] # removing Northamptonshire. This one has to be treated differently. 

    #### looping through each county code in the dataset, and changing that county code to one of (any of) its constituent local authorities
for(i in counties_to_replace){
  
  la_to_replace <- la_county_lookup[cty21cd == i, lad21cd][1] # taking the  first local authority listed beside the county to replace, to replace that county. 
  
  pupil_data[new_las == i, new_las := la_to_replace]
  
}

    #### for Northampton, we need to just change it manually to one of its constituent local authorities. Because E10000021 was split into E06000061 and E06000062 (Northamptonshire was split into North and West). The only solution I can think of code them both as Northampton before it was split, either with a new code or the pre-split code.

pupil_data[new_las == "E10000021", new_las := "E06000061"]


  ### 4.2. joining and aggregating
pupil_data <- aggregate_geographies_2(
  data = pupil_data, 
  lookup = lookup,
  geog_from_data = "new_las",
  geog_from_lookup = "lad21cd",
  geog_to_lookup = "itl221cd",
  count_names = c("full_time", "part_time", "headcount",  "fte")
)



  ## 5. aggregating to regional and national, and binding them on to the same dataset

  ### 5.1. reading in the lookup
itl_region_lookup <- fread("lookups/itl2_region_lookup.csv")

itl_region_lookup <- itl_region_lookup[, c("itl221cd", "itl121cd")]

  ### 5.2. aggregating to region
pupil_data_region <- aggregate_geographies_2(
  data = pupil_data,
  lookup = itl_region_lookup,
  geog_from_data = "itl221cd",
  geog_from_lookup = "itl221cd",
  geog_to_lookup = "itl121cd",
  count_names = c("full_time", "part_time", "headcount", "fte")
)

colnames(pupil_data_region)[colnames(pupil_data_region) == "itl121cd"] <- "itl221cd" # TO DO - I am actually misnaming the column here. What I should do is have one column that describes the type of geography is - ITL221, ITL121, etc - and then another that gives the code. 


  ### 5.3 aggregating to all England
itl_region_lookup[, country_code := "E92000001"]

region_country_lookup <- itl_region_lookup[, c("itl121cd", "country_code")]
region_country_lookup <- unique(region_country_lookup) # maybe it would be better to have this as a line within aggregate_geographies_2

pupil_data_england <- aggregate_geographies_2(
  data = pupil_data_region,
  lookup = region_country_lookup,
  geog_from_data = "itl221cd",
  geog_from_lookup = "itl121cd",
  geog_to_lookup = "country_code",
  count_names = c("full_time", "part_time", "headcount", "fte")
)

colnames(pupil_data_england)[colnames(pupil_data_england) == "country_code"] <- "itl221cd" # TO DO - again, this isn't right. Need two columns for geography


  ### 5.4 binding them all into the same dataset

pupil_data_all <- rbind(pupil_data, pupil_data_region, pupil_data_england)


  ## 6. saving the final file
output_filename <- paste0("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_", final_school_period, ".csv")

fwrite(x = pupil_data_all,
       file = output_filename)


rm(list = ls())
gc()
gc()
gc()



### https://l-hodge.github.io/ukgeog/articles/boundary-changes.html
### the page above tracks boundary changes in LAs, and it accounts for every troublemaking LA in the dataset. Very useful. 

### there were changes in 2023 that the above page doesn't account for. 
### E10000006, E10000023, and E10000027 ceased to exist. E06000063, E06000064, E06000065, and E06000066 came into existence. 
### what I did was just map the new ones to the right ITLs, by adding new rows into the LA-ITL lookup. This means the intermediate output here after mapping all LAs onto the most recent LAs isn't quite right - it's not a consistent series of LAs. Ideally I would sort this out. 
### but it doesn't matter, because all we need is a consistent series for ITLs. Which we now have. 


