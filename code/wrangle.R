# Code for data wrangling 

######### Load relevant packages #####
library(httr2)
library(tidyverse)
library(janitor)
library(stringr)
library(jsonlite)
library(purrr)
library(lubridate)
library(tidycensus)


####  Download population data #####

# Note to TF: Save census-key.R script in the main directory

source("./code/census-key.R")

url <- "https://api.census.gov/data/2021/pep/population"

population <- request(url) %>%
  req_url_query(get = I("POP_2020,POP_2021,NAME"), 
                `for` = I("state:*"), 
                key = census_key) %>%
  req_perform %>%
  resp_body_json(simplifyVector = TRUE)

#### Population data wrangling ####

population <- population %>% 
  row_to_names(1) %>%
  as_tibble() %>%
  select(-state) %>%
  rename(state_name = NAME) %>%
  pivot_longer(-state_name, names_to = "year", values_to = "population") %>%
  mutate(year = str_remove(year, "POP_")) %>%
  mutate(across(-state_name, as.numeric)) %>%
  mutate(state = state.abb[match(state_name, state.name)]) %>%
  mutate(state = case_when(state_name == "Puerto Rico" ~ "PR",
                           state_name == "District of Columbia" ~ "DC",
                           TRUE ~ state))

#### Load in population data
pop23 <- read.csv("raw_data/NST-EST2023-ALLDATA.csv")

### Keep Relevant columns, calculate 2024 population and pivot
pop23 <- pop23 |> 
  filter(STATE!=0) |>
  select(NAME, POPESTIMATE2022, POPESTIMATE2023
         ) |>
  rename(state_name = NAME) 

population_long <- pop23 %>%
  pivot_longer(
    cols = starts_with("POPESTIMATE"),
    names_to = "year",
    names_prefix = "POPESTIMATE",
    values_to = "population"
  ) |>
  mutate(year = as.numeric(year)) |>
 mutate(state = state.abb[match(state_name, state.name)]) |>
  mutate(state = case_when(state_name == "Puerto Rico" ~ "PR",
                           state_name == "District of Columbia" ~ "DC",
                           TRUE ~ state))

population_2024 <- population_long %>%
  group_by(state,state_name) %>%
  do({
    model <- lm(population ~ year, data = .)
    data.frame(year = 2024, population = predict(model, newdata = data.frame(year = 2024)))
  })

# Combine with original data
population_projected <- bind_rows(population_long, population_2024)



fullpop <- bind_rows(population, population_projected)


#### Download regional data and clean it ####

url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"

#  use jsonlit JSON parser 
regions <- fromJSON(url, simplifyDataFrame  = FALSE)

regions <- map_df(regions, function(x) 
  data.frame(region = x$region, region_name= x$region_name, state_name = x$states)) %>%
  mutate(region_name = 
           ifelse(region_name == "New York and New Jersey, Puerto Rico, Virgin Islands", "NY,NJ,PR,VI", region_name))


####  Population data with region ####
population <- left_join(fullpop, regions, by = "state_name")


#####  Run the get_cdc_function to output raw dataframes #####

source("./code/funcs.R")

hosp_raw <- get_cdc_data("https://data.cdc.gov/resource/39z2-9zu6.json")
deaths_raw <- get_cdc_data("https://data.cdc.gov/resource/r8kw-7aab.json")

#Hospitalizations reported daily, location given by variable "jurisdiction", time vars are collection_date
#Deaths reported weekly, location given by variable "state", time vars are Start_date, End_date, year, mmwr_week, week_ending_date, month


#####  Wrangling hosp_raw data  #####

#Keep only states for which we have population estimates. 
#Collapse the data into weekly data 
#Keep state MMWR year, MMWR week, and the total number of hospitalization for that week in that state.                 
#Remove weeks with less than 7 days reporting. 

hosp_clean <- hosp_raw %>% 
          filter(jurisdiction %in% population$state) %>%
          mutate(state = jurisdiction,
                 date = as_date(ymd_hms(collection_date)),
                 mmwr_year = epiyear(date),
                 mmwr_week = epiweek(date),
                 hosp = as.numeric(new_covid_19_hospital)) %>%
          select(state, mmwr_week, mmwr_year, hosp) %>%
          group_by(state, mmwr_year, mmwr_week) %>%
          summarise(hosp = sum(hosp, na.rm = TRUE),
                    count_days = n() ) %>%
          filter(!(count_days < 7))%>%
          select(-count_days) %>%
         ungroup()

##### Wrangling deaths_raw data #####
#Keep only states for which we have population estimates. 
#Keep state MMWR year, MMWR week, and the total number of deaths for that week in that state.                 

deaths_clean <- deaths_raw %>% 
  filter(state %in% population$state_name) %>%
  mutate(state_name = state,
         date = as_date(ymd_hms(end_date)),
         mmwr_year = epiyear(date),
         mmwr_week = as.numeric(mmwr_week),
         deaths= as.numeric(covid_19_deaths)) %>%
  select(state_name, mmwr_week, mmwr_year, deaths) 


#####  Merging all datasets to create the dat file  #####
## Make dates data frame
all_dates <- data.frame(date = seq(make_date(2020, 1, 25),
                                   make_date(2024, 12, 31), 
                                   by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |>
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date)) 

dates_and_pop <- cross_join(all_dates, data.frame(state = unique(population$state))) %>%
                left_join(population, by = c("state", "mmwr_year" = "year"))

## Merge
dat <- dates_and_pop %>%

  left_join(hosp_clean, by = c("state", "mmwr_year", "mmwr_week")) %>%
  left_join(deaths_clean, by = c("state_name", "mmwr_year", "mmwr_week")) %>%
  
  arrange(state, mmwr_year, mmwr_week) %>%
  mutate( across(c(hosp, deaths),as.numeric) 
  )


## Output as RDS
saveRDS(dat, file = "./data/dat.rds")

