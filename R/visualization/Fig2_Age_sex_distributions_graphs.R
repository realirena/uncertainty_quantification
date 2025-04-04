
# ---------------------------------------------------------------------------- #
# Title: Sex-age distributions plot
#
# Code to generate plot comparing the different sex-age distributions
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, packages and functions
#   1. Create plot
#   2. Save plot
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #

source("R/0_setup.R")
rm(list = ls())

# Read all distributions
pi_x_moh_23 <- read_rds("data/pi_x_moh_2023.rds") %>% mutate(scenario = NA)
pi_x_moh_24 <- read_rds("data/pi_x_moh_2024.rds")  %>% mutate(scenario = NA)
pi_x_moh_23_gaza <- read_rds("data/pi_x_moh_2023_gaza.rds")  %>% mutate(scenario = NA)
pi_x_moh_24_gaza <- read_rds("data/pi_x_moh_2024_gaza.rds") %>% mutate(scenario = NA)
pi_x_un_23 <- read_rds("data/pi_x_un_2023.rds")
pi_x_un_24 <- read_rds("data/pi_x_un_2024.rds")
pi_x_un_23_gaza <- read_rds("data/pi_x_un_2023_gaza.rds")
pi_x_un_24_gaza <- read_rds("data/pi_x_un_2024_gaza.rds")
pi_x_btselem_23 <- read_rds("data/pi_x_btselem_2023.rds")  %>% mutate(scenario = NA)
pi_x_btselem_24 <- read_rds("data/pi_x_btselem_2024.rds")  %>% mutate(scenario = NA)
pi_x_btselem_23_gaza <- read_rds("data/pi_x_btselem_2023_gaza.rds")  %>% mutate(scenario = NA)
pi_x_btselem_24_gaza <- read_rds("data/pi_x_btselem_2024_gaza.rds")  %>% mutate(scenario = NA)

# Paste all distribution together
pi_x <- rbind(pi_x_moh_23, pi_x_moh_24, pi_x_moh_23_gaza, pi_x_moh_24_gaza, pi_x_btselem_23, pi_x_btselem_24,
              pi_x_btselem_23_gaza, pi_x_btselem_24_gaza, pi_x_un_23, pi_x_un_24, pi_x_un_23_gaza, pi_x_un_24_gaza)
# write.csv(pi_x, file = "data/age_sex_distributions.csv")

# ---------------------------------------------------------------------------- #
#     1. Create plot
# ---------------------------------------------------------------------------- #

facet_labels <- c("BTselem" = "B'Tselem historical\naverage", 
                  "MoH" = "GMoH report", 
                  "UN-IGME" = "UN-IGME genocide\npattern")

# Figure 2 in main text
age_sex_distributions <- pi_x %>%
  filter(sex != "t" & year == 2023 & scenario %in%c("genocide", NA) & region == "Palestine") %>%
  ggplot() +
  # geom_point(aes(x = age, y = pi_x_mean, colour = sex)) + 
  geom_line(aes(x=age, y=pi_x_mean, colour = sex),  linewidth=1.25) + 
  geom_ribbon(aes(x=age, y=pi_x_mean, ymin = pi_x_ll, ymax = pi_x_ul, fill = sex),  size=1.25, alpha = 0.3) +
  facet_grid( ~ source, labeller = labeller(source = facet_labels)) + 
  theme_bw() + xlab("Age") + ylab("Proportion") + 
  scale_x_continuous(breaks = seq(0,80, 20), labels = c(seq(0,60,20),"80+")) +
  scale_colour_manual(values = c( "#c44536", "#283d3b"), name = "Sex", labels = c("Females", "Males")) + 
  scale_fill_manual(values = c( "#c44536","#283d3b"), name = "Sex", labels = c("Females", "Males"), guide = "none") + 
  theme(strip.text = element_text(size = 13, face="bold"), 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), 
        legend.position = "bottom",
        strip.background = element_blank())

## All distributions for supplementary material
age_sex_distributions_all <- pi_x %>%
  filter(sex != "t" & region == "Palestine") %>%
  mutate(distr = case_when(source == "MoH" & year == 2023 ~ "GMoH report (List 1)",
                           source == "MoH" & year == 2024 ~ "GMoH report (List 7)",
                           source == "BTselem" & year == 2023 ~ "B'Tselem historical\naverage",
                           source == "UN-IGME" &  scenario == "genocide" & year == 2023 ~ "UN-IGME genocide\npattern",
                           source == "UN-IGME" &  scenario == "conflict" & year == 2023 ~ "UN-IGME conflict\npattern",
                           source == "UN-IGME" &  scenario == "earthquake" & year == 2023 ~ "UN-IGME earthquake\npattern",
                           TRUE ~ NA)) %>%
  filter(!is.na(distr)) %>%
  ggplot() +
  # geom_point(aes(x = age, y = pi_x_mean, colour = sex)) + 
  geom_line(aes(x=age, y=pi_x_mean, colour = sex),  linewidth=1.25) + 
  geom_ribbon(aes(x=age, y=pi_x_mean, ymin = pi_x_ll, ymax = pi_x_ul, fill = sex),  size=1.25, alpha = 0.3) +
  facet_wrap( ~ distr) + 
  theme_bw() + xlab("Age") + ylab("Proportion") + 
  scale_x_continuous(breaks = seq(0,80, 20), labels = c(seq(0,60,20),"80+")) +
  scale_colour_manual(values = c( "#c44536", "#283d3b"), name = "Sex", labels = c("Females", "Males")) + 
  scale_fill_manual(values = c( "#c44536","#283d3b"), name = "Sex", labels = c("Females", "Males"), guide = "none") + 
  theme(strip.text = element_text(size = 13, face="bold"), 
        axis.text = element_text(size = 11), 
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), 
        legend.position = "bottom",
        strip.background = element_blank())


# ---------------------------------------------------------------------------- #
#     2. Save plot
# ---------------------------------------------------------------------------- #

# ggsave(age_sex_distributions, file = "figures/age_sex_distributions.pdf", width = 6.6, height = 3.9)
 
# ggsave(age_sex_distributions_all, file = "figures/age_sex_distributions_all.pdf", width = 6.6, height = 7.8)

