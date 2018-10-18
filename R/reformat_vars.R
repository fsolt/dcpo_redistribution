library(tidyverse)
library(stringr)

reformat_vars <- function(vars_file) {
	df <- read_csv(vars_file) %>% 
		rename(item_old = item) %>% 
		mutate(item = str_replace(item_old, "_.*", "")) %>% 
		group_by(item) %>% 
		mutate(revval = (value == max(value) & reverse == TRUE),
					 minval = if_else(!revval, min(value), max(value)),
					 maxval = if_else(!revval, max(value), min(value)),
					 values = str_c("c(", minval, ":", maxval, ")"),
					 dich = (minval == maxval)) %>% 
		filter(value == max(value)) %>%
		arrange(-dich) %>% 
		select(survey, item, variable, values, to_na, dich)
	cat("Remember to recode dichotomous variables by hand!")
	write_csv(df, 
						path = str_c(dirname(vars_file), "/new_", basename(vars_file)),
						na = "")
	return(df)
}

redist_vars <- reformat_vars("data-raw/old_redist_vars.csv")
	
	
