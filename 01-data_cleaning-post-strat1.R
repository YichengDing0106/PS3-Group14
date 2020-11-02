#### Preamble ####
# Purpose: Prepare and clean the ACS census data downloaded from IPUMS USA
# Authors: Xiaoxuan Han, Yicheng Ding, April Ding, Amber Kao
# Data: 2 November 2020
# Contact: amber.kao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data


#### Workspace setup ####
library(tidyverse)
# Read in the raw data.
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00001.xml")
raw_data <- read_ipums_micro(ddi)

# setwd(dir = "C:/Users/amber/Desktop/Academic/4thYear/STA304/R/")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(SEX, 
         AGE, 
         REGION, 
         RACE, 
         EDUCD)
         

#### What's next? ####

reduced_data <- 
  reduced_data %>% 
  filter(AGE != "Less than 1 year old") %>%
  filter(AGE != "90 (90+ in 1980 and 1990)") %>% 
  filter(AGE != "100 (100+ in 1960-1970)") %>%
  filter(AGE != "112 (112+ in the 1980 internal data)") %>%
  filter(AGE != "115 (115+ in the 1990 internal data)")

reduced_data$AGE <- as.integer(reduced_data$AGE)

reduced_data <- 
  reduced_data %>% 
  filter(AGE >= 18)

reduced_data <- 
  reduced_data %>% 
  mutate(
    AGE = case_when(
      AGE >= 18 & AGE <= 29 ~ "18-29", 
      AGE >= 30 & AGE <= 44 ~ "30-44", 
      AGE >= 45 & AGE <= 64 ~ "45-64", 
      AGE >= 65 ~ "65+"))

reduced_data <- 
  reduced_data %>% 
  mutate(
    REGION = case_when(
      REGION == "New England Division" ~ "Northeast",
      REGION == "Middle Atlantic Division" ~ "Northeast",
      REGION == "East North Central Div." ~ "Midwest",
      REGION == "West North Central Div." ~ "Midwest",
      REGION == "South Atlantic Division" ~ "South",
      REGION == "East South Central Div." ~ "South",
      REGION == "West South Central Div." ~ "South",
      REGION == "Mountain Division" ~ "West",
      REGION == "Pacific Division" ~ "West"))

reduced_data <- 
  reduced_data %>% 
  mutate(
    RACE = case_when(
      RACE == "White" ~ "White",
      RACE == "Black/African American/Negro" ~ "Black or African American",
      RACE == "American Indian or Alaska Native" ~ "American Indian or Alaska Native", 
      RACE == "Chinese" ~ "Chinese",
      RACE == "Japanese" ~ "Japanese",
      RACE == "Other Asian or Pacific Islander" ~ "Other Asian or Pacific Islander",
      RACE == "Other race, nec" ~ "Other race, or two or more races",
      RACE == "Two major races" ~ "Other race, or two or more races",
      RACE == "Three or more major races" ~ "Other race, or two or more races"))

reduced_data <- 
  reduced_data %>% 
  mutate(
    EDUCD = case_when(
      EDUCD == "No schooling completed" ~ "3rd Grade or less",
      EDUCD == "Nursery school, preschool" ~ "3rd Grade or less",
      EDUCD == "Grade 1" ~ "3rd Grade or less",
      EDUCD == "Grade 2" ~ "3rd Grade or less",
      EDUCD == "Grade 3" ~ "3rd Grade or less",
      EDUCD == "Kindergarten" ~ "3rd Grade or less",
      EDUCD == "Grade 4" ~ "4th to 8th Grade",
      EDUCD == "Grade 5" ~ "4th to 8th Grade",
      EDUCD == "Grade 6" ~ "4th to 8th Grade",
      EDUCD == "Grade 7" ~ "4th to 8th Grade",
      EDUCD == "Grade 8" ~ "4th to 8th Grade",
      EDUCD == "Grade 9" ~ "High school, no degree",
      EDUCD == "Grade 10" ~ "High school, no degree",
      EDUCD == "Grade 11" ~ "High school, no degree",
      EDUCD == "12th grade, no diploma" ~ "High school, no degree",
      EDUCD == "GED or alternative credential" ~ "High school diploma or GED",
      EDUCD == "Regular high school diploma" ~ "High school diploma or GED",
      EDUCD == "Some college, but less than 1 year" ~ "College, no degree",
      EDUCD == "1 or more years of college credit, no degree" ~ "College, no degree",
      EDUCD == "Associate's degree, type not specified" ~ "Associate's degree",
      EDUCD == "Bachelor's degree" ~ "Bachelor's degree",
      EDUCD == "Master's degree" ~ "Master's degree",
      EDUCD == "Doctoral degree" ~ "Doctorate or professional degree beyond bachelor's",
      EDUCD == "Professional degree beyond a bachelor's degree" ~ "Doctorate or professional degree beyond bachelor's"))

## Splitting cells by the five variables
reduced_data_cells <- 
  reduced_data %>%
  count(SEX, AGE, REGION, RACE, EDUCD) %>%
  group_by(SEX, AGE, REGION, RACE, EDUCD)

reduced_data_cells <- 
  reduced_data_cells %>% 
  rename(gender = SEX) %>% 
  rename(age = AGE) %>% 
  rename(census_region = REGION) %>% 
  rename(race_ethnicity = RACE) %>% 
  rename(education = EDUCD)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data_cells, "census_data.csv")



         