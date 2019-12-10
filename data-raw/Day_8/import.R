
# Data Extract script -----------------------------------------------------

img_string <- here::here("data-raw/Day_8/input.txt") %>%
        readLines()

use_data(img_string)
