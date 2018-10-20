library(tidyverse)
library(DCPO)

red <- dcpo_setup(vars = "data-raw/surveys_redist.csv",
                  file = "data/all_data_redist.csv")
