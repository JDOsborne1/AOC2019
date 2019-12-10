
# Day 6 Data Munge --------------------------------------------------------

orbit_input <- here::here("data-raw/Day_6/input.txt") %>%
        vroom::vroom(col_names = "Orbits")

use_data(orbit_input)
