library(tidyverse)
library(DCPO)

# red <- dcpo_setup(vars = "data-raw/surveys_redist.csv",
# 									file = "data/all_data_redist.csv")

red <- dcpo_setup(vars = read_csv("data-raw/surveys_redist.csv", col_types = "ccccc"))

