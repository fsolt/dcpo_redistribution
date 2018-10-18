# estimate plus raw data, eight countries

countries <- c("Germany", "United States", "Sweden", "Finland", 
							 "Luxembourg", "United Kingdom", "Spain", "Denmark")
c8_rd <- red %>% filter(country %in% countries) %>% mutate(estimate = y_r/n) %>%
	left_join(a_res, by = c("rcode", "variable_cp")) %>% rename(alpha_mean = mean)
c8_res <- t_res %>% filter(country %in% countries)

p_rd <- ggplot(data = c8_rd, aes(x = year, y = estimate)) +
	geom_text(aes(label = rcode), alpha = .5) + theme_bw() +
	theme(legend.position="none") +
	coord_cartesian(xlim = c(1975, 2016), ylim = c(0, 1)) +
	labs(x = NULL, y = "Demand for Equality") +
	geom_ribbon(data = c8_res, aes(ymin = lb, ymax = ub, linetype=NA), alpha = .25) +
	geom_line(data = c8_res) +
	facet_wrap(~country, ncol = 4) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1),
				strip.background = element_rect(fill = "white", colour = "white"))

ggsave("paper/figures/8c.pdf", plot = p_rd, width = 6, height = 4)

p_rd2 <- ggplot(data = c8_rd, aes(x = year, y = estimate)) +
	geom_line(aes(color = variable, alpha = gamma_mean)) + 
	geom_point(aes(color = variable, alpha = gamma_mean)) + theme_bw() +
	theme(legend.position="none") +
	coord_cartesian(xlim = c(1975, 2016), ylim = c(0, 1)) +
	labs(x = NULL, y = "Demand for Redistribution") +
	geom_ribbon(data = c8_res, aes(ymin = lb, ymax = ub, linetype=NA), alpha = .25) +
	geom_line(data = c8_res) +
	facet_wrap(~country, ncol = 4) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1),
				strip.background = element_rect(fill = "white", colour = "white"))

ggsave("paper/figures/SoltDonnelly8c_line.pdf", plot = p_rd2, width = 6, height = 4)


# nine countries, estimates only, for RSF proposal
red <- read_csv("data/all_data_red.csv")
red %>% group_by(country) %>% summarise(count = n()) %>% arrange(-count)
countries <- red %>%
	group_by(country) %>% 
	summarise(count = n()) %>% 
	arrange(-count) %>% 
	head(n = 9) %>% 
	select(country) %>%
	unlist()

c9_rd <- red %>% filter(country %in% countries) %>% mutate(estimate = y_r/n) %>%
	left_join(g_res, by = c("rcode", "variable")) %>% rename(gamma_mean = mean)
c9_rd$c2 = factor(c9_rd$country, levels=c("United States", sort(setdiff(countries, "United States"))))

c9_res <- a_res %>% filter(country %in% countries)
c9_res$c2 = factor(c9_res$country, levels=c("United States", sort(setdiff(countries, "United States"))))

p_rd3 <- ggplot(data = c9_rd, aes(x = year, y = estimate)) +
	# geom_line(aes(color = variable, alpha = gamma_mean)) + 
	# geom_point(aes(color = variable, alpha = gamma_mean)) + 
	theme_bw() +
	theme(legend.position="none") +
	coord_cartesian(xlim = c(1975, 2016), ylim = c(0, 1)) +
	labs(x = NULL, y = "Demand for Redistribution") +
	geom_ribbon(data = c9_res, aes(ymin = lb, ymax = ub, linetype=NA), alpha = .25) +
	geom_line(data = c9_res) +
	facet_wrap(~c2, ncol = 3) +
	theme(axis.text.x = element_text(angle = 45, hjust = 1),
				strip.background = element_rect(fill = "white", colour = "white"))

ggsave("paper/figures/SoltDonnelly_9c.pdf", plot = p_rd3, width = 6, height = 6)
