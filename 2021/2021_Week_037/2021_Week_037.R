# TidyTuesday - 2021 - Week 37
## Formula 1 Races data set
###  Week     Date	           Data	                 Source	                 Article
###  - 37     - 2021-09-07     - Formula 1 Races     - ergast.com/mrd/db     - FiveThirtyEight

# Libraries
library(tidytuesdayR)
library(tidyverse)

# Get the Data
tt_data <- tt_load(2021, week = 37)

results <- tt_data$results  