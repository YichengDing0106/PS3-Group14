#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund Voter Study Group
# Authors: Xiaoxuan Han, Yicheng Ding, April Ding, Amber Kao
# Data: 2 November 2020
# Contact: amber.kao@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from Democracy Fund Voter Study Group 
# and save the folder that you're interested in to inputs/data 


#### Workspace setup ####
library(haven)
library(tidyverse)
# setwd(dir = "C:/Users/amber/Desktop/Academic/4thYear/STA304/R/")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data1 <- read_dta("ns20200625/ns20200625.dta")
# Add the labels
raw_data1 <- labelled::to_factor(raw_data1)
# Just keep some variables
reduced_data1 <- 
  raw_data1 %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         gender,
         age,
         census_region,
         race_ethnicity,
         education)

reduced_data1 <- 
  reduced_data1 %>% 
  filter(registration == "Registered") %>% 
  select(-registration) %>% 
  na.omit()

reduced_data1 <- 
  reduced_data1 %>% 
  mutate(
    age = case_when(
      age >= 18 & age <= 29 ~ "18-29", 
      age >= 30 & age <= 44 ~ "30-44", 
      age >= 45 & age <= 64 ~ "45-64", 
      age >= 65 ~ "65+"
    )
  )

reduced_data1 <- 
  reduced_data1 %>% 
  mutate(
    race_ethnicity = case_when(
      race_ethnicity == "White" ~ "White", 
      race_ethnicity == "Black, or African American" ~ "Black or African American", 
      race_ethnicity == "American Indian or Alaska Native" ~ "American Indian or Alaska Native", 
      race_ethnicity == "Asian (Chinese)" ~ "Chinese", 
      race_ethnicity == "Asian (Japanese)" ~ "Japanese", 
      race_ethnicity == "Asian (Asian Indian)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Asian (Filipino)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Asian (Korean)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Asian (Other)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Asian (Vietnamese)" ~ "Other Asian or Pacific Islander",  
      race_ethnicity == "Pacific Islander (Guamanian)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Pacific Islander (Other)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Pacific Islander (Samoan)" ~ "Other Asian or Pacific Islander", 
      race_ethnicity == "Some other race" ~ "Other race, or two or more races"
    )
  )

reduced_data1 <- 
  reduced_data1 %>% 
  mutate(
    education = case_when(
      education == "3rd Grade or less" ~ "3rd Grade or less", 
      education == "Associate Degree" ~ "Associate's degree", 
      education == "College Degree (such as B.A., B.S.)" ~ "Bachelor's degree", 
      education == "Completed some college, but no degree" ~ "College, no degree", 
      education == "Completed some graduate, but no degree" ~ "Bachelor's degree", 
      education == "Completed some high school" ~ "High school, no degree", 
      education == "Doctorate degree" ~ "Doctorate or professional degree beyond bachelor's", 
      education == "High school graduate" ~ "High school diploma or GED", 
      education == "Masters degree" ~ "Master's degree", 
      education == "Middle School - Grades 4 - 8" ~ "4th to 8th Grade", 
      education == "Other post high school vocational training" ~ "High school diploma or GED"
    )
  )

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data1, "survey_data.csv")

