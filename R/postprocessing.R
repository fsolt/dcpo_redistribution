library(rstan)
library(DCPO)
library(tidyverse)

#Postprocessing
x1 <- summary(out1)
write.table(as.data.frame(x1$summary), file="x1.csv", sep = ",")
x1_sum <- as.data.frame(x1$summary)
x1_sum$parameter <- rownames(x1_sum)
# x1_sum <- read_csv("x1.csv")
# names(x1_sum)[2] <- "mean"
x1_sum$parameter_type <- gsub("([^[]*).*", "\\1", x1_sum$parameter)
View(x1_sum)
View(x1_sum[x1_sum$Rhat>1.1,])
View(x1_sum[x1_sum$Rhat>1.2,])

ggplot(x1_sum) +
	aes(x = parameter_type, y = Rhat, color = parameter_type) +
	geom_jitter(height = 0, width = .5, show.legend = FALSE, alpha = .2) +
	ylab(expression(hat(italic(R))))
ggsave(paste0("rhat_", iter, ".pdf"))




qcodes <- x %>% group_by(variable) %>%
	summarize(qcode = first(qcode),
						r_n = n()) %>%
	arrange(qcode)

rcodes <- x %>% group_by(variable_cp) %>%
	summarize(rcode = first(rcode),
						r_n = n()) %>%
	arrange(rcode)



b_res <- x1_sum %>%
	filter(parameter_type=="beta") %>%
	select(parameter, mean, `2.5%`, `97.5%`, Rhat) %>%
	mutate(rcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
	left_join(rcodes, by="rcode")
a_res <- x1_sum %>% filter(parameter_type=="alpha") %>%
	select(parameter, mean, `2.5%`, `97.5%`, Rhat) %>%
	mutate(rcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
	left_join(rcodes, by="rcode")
# s_dk_res <- x1_sum %>% filter(parameter_type=="sd_k") %>% select(parameter, mean, `2.5%`, `97.5%`)  %>% mutate(rcode = as.numeric(str_extract(parameter, "\\d+"))) %>% left_join(rcodes)

beep()

ktcodes <- x %>%
	group_by(country) %>%
	mutate(firstyr = first(firstyr),
				 lastyr = first(lastyr)) %>%
	ungroup() %>%
	select(ktcode, ccode, tcode, country, year, firstyr, lastyr, year_obs) %>%
	unique() %>%
	right_join(tibble(ktcode = 1:(max(x$ccode)*max(x$tcode)),
										ccode = rep(1:max(x$ccode), each = max(x$tcode)),
										tcode = rep(1:max(x$tcode), times = max(x$ccode)),
										year = rep(min(x$year):max(x$year), times = max(x$ccode))),
						 by = c("ktcode", "ccode", "tcode", "year")) %>%
	group_by(ccode) %>%
	fill(everything()) %>%
	fill(everything(), .direction = "up") %>%
	ungroup()


t_res <- summary(out1, pars="theta", probs=c(.1, .9)) %>%
	first() %>%
	as.data.frame() %>%
	rownames_to_column("parameter") %>%
	as_tibble() %>%
	mutate(ktcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
	left_join(ktcodes, by="ktcode")

k <- x %>%
	group_by(country) %>%
	summarize(ccode = first(ccode),
						firstyr = first(firstyr),
						lastyr = first(lastyr)) %>%
	ungroup()

t_res$year <- min(t_res$firstyr) + t_res$tcode - 1
t_res1 <- t_res %>%
	#  filter(year <= lastyr & year >= firstyr) %>%
	transmute(year_obs = year_obs,
						firstyr = firstyr,
						lastyr = lastyr,
						country = country,
						term = country,
						kk = ccode,
						year = year,
						estimate = mean,
						lb = `10%`,
						ub = `90%`) %>%
	arrange(kk, year)


# Plots:
#   1. tolerance by country, most recent available year: cs_plot
#   2. tolerance trends, estimate plus raw data, eight countries
#   3. trends in all countries: ts_plot
#   4. probability of tolerant answer by tolerance (beta and gamma), selected items (modelled on McGann2014, fig 1)
#   5. bar chart of beta and gamma for all items?

