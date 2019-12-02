
# Data Import for Day 1: Part 1 -------------------------------------------

library(dplyr)

Day_1_masses <- here::here("data-raw/Day_1/input1.txt") %>%
        vroom::vroom(col_names = "Mass")
use_data(Day_1_masses, overwrite = T)
