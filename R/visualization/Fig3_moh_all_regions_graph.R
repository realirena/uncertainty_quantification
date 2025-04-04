
# ---------------------------------------------------------------------------- #
# Title:  Plot of life expectancy by region (Figure 3)
#
# Code to create plot of life expectancy estimates from 2012-2024 by region
# (figure 3 in the main text)
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Load data
#   2. Plot
#   3. Save plot
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)

# ---------------------------------------------------------------------------- #
#     1. Load data
# ---------------------------------------------------------------------------- #

## Life expectancy estimates 2012-2019
dt_ex <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019") %>%
  mutate(scenario="Counterfactual with\n no conflict deaths", 
         lss = ex_noc - ex_cnf,
                  sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))
  
## Results directory for Gaza and Palestine estimates
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## Life expectancy estimates Palestine 2023 
nat_le_m_age0 <- read.csv(paste0(results_dir, "palestine/moh23_lifetable_m_le0.csv"))
nat_le_f_age0 <- read.csv(paste0(results_dir, "palestine/moh23_lifetable_f_le0.csv"))
nat_le_t_age0 <- read.csv(paste0(results_dir, "palestine/moh23_lifetable_t_le0.csv"))

nat_all <- rbind(nat_le_f_age0[,vars], nat_le_m_age0[,vars], nat_le_t_age0[,vars])
nat_all$region="Palestine"

## Life expectancy estimates Gaza 2023 
gaza_le_m_age0 <- read.csv(paste0(results_dir, "gaza/moh23_lifetable_m_le0.csv"))
gaza_le_f_age0 <- read.csv(paste0(results_dir, "gaza/moh23_lifetable_f_le0.csv"))
gaza_le_t_age0 <- read.csv(paste0(results_dir, "gaza/moh23_lifetable_t_le0.csv"))

gaza_all <- rbind(gaza_le_f_age0[,vars], gaza_le_m_age0[,vars], gaza_le_t_age0[,vars])
gaza_all$region="Gaza Strip"

## Life expectancy estimates West Bank 2023 
wb_all <- read_rds("R/model/samples/pcbs_2019/2023/west_bank/lifetable_age0_wb_23_v2.rds")
wb_all$region="West Bank"

# All estimates for 2023
le_lss_all <- rbind(nat_all, gaza_all, wb_all) 


## Life expectancy estimates Palestine 2024
nat_le_m_24 <- read.csv(paste0(results_dir, "palestine/moh24_lifetable_m_le0.csv"))
nat_le_f_24 <- read.csv(paste0(results_dir, "palestine/moh24_lifetable_f_le0.csv"))
nat_le_t_24 <- read.csv(paste0(results_dir, "palestine/moh24_lifetable_t_le0.csv"))

nat_24_all  <- rbind(nat_le_m_24[,vars], nat_le_f_24[,vars],nat_le_t_24[,vars])
nat_24_all$region="Palestine"

## Life expectancy estimates Gaza 2024
gaza_le_m_24 <- read.csv(paste0(results_dir, "gaza/moh24_lifetable_m_le0.csv"))
gaza_le_f_24 <- read.csv(paste0(results_dir, "gaza/moh24_lifetable_f_le0.csv"))
gaza_le_t_24 <- read.csv(paste0(results_dir, "gaza/moh24_lifetable_t_le0.csv"))

gaza_24_all <- rbind(gaza_le_f_24[,vars], gaza_le_m_24[,vars], gaza_le_t_24[,vars])
gaza_24_all$region="Gaza Strip"

## Life expectancy estimates West Bank 2024 
wb_24_all <- read_rds("R/model/samples/pcbs_2019/2024/west_bank/lifetable_age0_wb_24_v2.rds")
wb_all$region="West Bank"

# All estimates for 2024
le_lss_all24 <- rbind(nat_24_all, gaza_24_all, wb_24_all)


# ---------------------------------------------------------------------------- #
#     2. Plot
# ---------------------------------------------------------------------------- #

## Define panels
panels <- tibble(sex = rep(c("Females", "Males", "Total"), 2), 
                 type = c(rep("Life expectancy \nat birth", 3), 
                          rep("Life expectancy \nloss to conflict", 3)),
                 panel = c("A", "B", "C", "D", "E", "F"),
                 ypos = c(rep(76, 3), rep(21, 3)))

## Life expectancy at birth 2012-2023
# Here we filter only for males and females, but the sex filter can be removed
le0_plot <- ggplot() +   
  geom_point(data = dt_ex %>% filter(year <= 2022) %>%
               filter(sex != "Total"),
             aes(x = year, y = ex_cnf,color=region, pch =  region),  alpha = 1, size = 3) + 
  geom_line(data = dt_ex%>%
              filter(sex != "Total"), aes(year, ex_noc, color=region, group=region, 
                                          linetype = "Life Expectancy (no conflict)"),alpha=0.5,  linewidth=1) + 
  # counter factual with no conflict deaths
  # geom_line(data = dt_ex %>% filter(year > 2022), 
  #            aes(x = year, y = ex_noc, color=region),  group = "Life Expectancy (no conflict)", alpha = 0.5, size = 1.5)+
  stat_histinterval(data=le_lss_all %>%
                      filter(sex != "Total"), aes(x = year, y=ex, group=region, 
                                         fill=region, color=region),
                    size=2,
                    alpha=0.5) +
  geom_point(data = le_lss_all %>%
               filter(sex != "Total") %>% filter(year >= 2023) %>%
               group_by(region, year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean, color=region, pch =  region),  alpha = 1, size = 3) + 
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) +
  scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Gaza Strip",
                       "Palestine",
                       "West Bank"),guide = 'none') + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"), name  = "Region") +
  xlab("Year") + ylab("Life expectancy at birth") +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  guides(shape = "none") +
  theme_bw()+
  scale_x_continuous(breaks = seq(2012, 2024, 2)) +
  # guides(color = guide_legend(override.aes = list(linetype = 0)),
  #        linetype = guide_legend(order = 1))+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,84),expand = c(0,0))

## Plot of life expectancy loss in 2012-2023
# Not used in figure 3
# lss_plot <-  ggplot() +   
#   geom_point(data = dt_ex  %>% filter(year <= 2022), 
#              aes(x = year, y = lss, color=region, pch =  region), alpha = 1, size = 2) +   
#   stat_histinterval(data=le_lss_all %>%filter(region != "West Bank"), aes(x = year, y=bmmr_lss, group=region, fill=region, color=region), alpha=0.5) +
#   # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
#   # scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
#   scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2"),
#                      labels = c( 
#                        "Gaza Strip",
#                        "Palestine",
#                        "West Bank")) + 
#   scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none') +
#   guides(shape = "none") +
#   facet_grid(~ sex, scale = "free_y", space = "free_y", switch = "y") +
#   theme_bw()+
#   guides(color = guide_legend(override.aes = list(linetype = 0)),
#          linetype = guide_legend(order = 1))+
#   theme(strip.background = element_blank(),
#         strip.placement = "outside",
#         strip.text = element_text(size = 13),
#         legend.position = "bottom",
#         legend.text = element_text(size = 12),
#         legend.title = element_text(size = 13),
#         axis.title.y = element_blank(),
#         axis.text = element_text(size = 11),
#         axis.title = element_text(size = 12))

# le0_lss <- gridExtra::grid.arrange(le0_plot, lss_plot)

# ggsave("figures/un_conflict_pix/le0_lss_palestine_23.png", plot=le0_lss, w = 16, h = 8)

### Add 2024 to the plots
# Life expectancy 2012-2024
le0_23_24  <- le0_plot +  stat_histinterval(data=le_lss_all24  %>%
                                              filter(sex != "Total"), aes(x = year, y=ex, group=region, 
                                                                   fill=region, color=region),
                               size=2,
                               alpha=0.5) +
  geom_point(data = le_lss_all24  %>%
               filter(sex != "Total") %>% filter(year >= 2023) %>%
               group_by(region, year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean,color=region, pch =  region),  alpha = 1, size = 3) 

# Life expectancy loss 2012-2024
# Not used in Figure 3
# lss_23_24  <-  lss_plot +  stat_histinterval(data=le_lss_all24, aes(x = year, y=bmmr_lss, group=region, 
#                                                     fill=region, color=region),
#                                size=2,
#                                alpha=0.5) 

# le0_lss_23_24 <- gridExtra::grid.arrange(le0_23_24, lss_23_24)

#ggsave("figures/le0_lss_palestine_23_24.png", plot=le0_lss_23_24, w = 16, h = 8)

### Creating zoomed-in plots

## Males zoomed-in plot
focus_male <- ggplot() + 
  stat_histinterval(data=le_lss_all %>% filter(region != "West Bank" & sex == "Males"), 
                    aes(x = year, y=ex, group=region, 
                        fill=region, color=region, pch =  region),
                    size=2,
                    alpha=0.5) +
  scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Gaza Strip",
                       "Palestine",
                       "West Bank"),guide = 'none') + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none') + xlab("") +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  guides(shape = "none", linetype = "none") +
  theme_bw()+
  scale_x_continuous(breaks = c(2023,2024)) +
  scale_y_continuous(breaks = seq(30,60,5)) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        plot.background = element_rect(color = NA, fill = "lightgrey", linewidth = 1), 
        panel.grid.minor = element_blank()) +
  stat_histinterval(data=le_lss_all24 %>% filter(region != "West Bank" & sex == "Males"), 
                    aes(x = year, y=ex, group=region, 
                        fill=region, color=region),
                    size=2,
                    alpha=0.5) +
  geom_point(data = rbind(le_lss_all, le_lss_all24) %>%
               filter(region != "West Bank" & sex == "Males") %>% filter(year >= 2023) %>%
               group_by(region, year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean, color=region, pch =  region),  alpha = 1, size = 3)

## Females zoomed-in plot
focus_female <- ggplot() + 
  stat_histinterval(data=le_lss_all %>% filter(region != "West Bank" & sex == "Females"), 
                    aes(x = year, y=ex, group=region, 
                        fill=region, color=region),
                    size=2,
                    alpha=0.5) +
  scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Gaza Strip",
                       "Palestine",
                       "West Bank"),guide = 'none') + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none') + xlab("") +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  guides(shape = "none", linetype = "none") +
  theme_bw()+
  scale_x_continuous(breaks = c(2023,2024)) +
  scale_y_continuous(breaks = seq(35,70,5)) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        axis.title = element_blank(),
        axis.text = element_text(size = 8),
        plot.background = element_rect(color = NA, fill = "lightgrey", linewidth = 1), 
        panel.grid.minor = element_blank()) +
  stat_histinterval(data=le_lss_all24 %>% filter(region != "West Bank" & sex == "Females"), 
                    aes(x = year, y=ex, group=region, 
                        fill=region, color=region),
                    size=2,
                    alpha=0.5) +
  geom_point(data = rbind(le_lss_all, le_lss_all24) %>%
               filter(region != "West Bank" & sex == "Females") %>% filter(year >= 2023) %>%
               group_by(region, year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean, color=region, pch =  region),  alpha = 1, size = 3)


## Function that allows us to specify which facet to annotate
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
  }

# Generate plot grobs
embedded_grob_m <- ggplotGrob(focus_male)
embedded_grob_f <- ggplotGrob(focus_female)

# Define where to insert each graph
rect_data_m <- data.frame(xmin = c(2022), xmax = c(2025), ymin = c(32), ymax = c(60), sex = "Males")
rect_data_f <- data.frame(xmin = c(2022), xmax = c(2025), ymin = c(39), ymax = c(67), sex = "Females")

## Combine the plots
le0_23_24_2 <- le0_23_24 +
  annotation_custom2(grob=embedded_grob_m, 
                     data = data.frame(sex="Males"),
                     xmin = 2013, xmax = 2020, ymin = 6, ymax = 61) +
  annotation_custom2(grob=embedded_grob_f, 
                     data = data.frame(sex="Females"),
                     xmin = 2013, xmax = 2020, ymin = 6, ymax = 61) +
  geom_rect(data = rect_data_m, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "black", linewidth = 0.5, linetype = 2)  +
  geom_rect(data = rect_data_f, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "black", linewidth = 0.5, linetype = 2)  +
  geom_segment(data = rect_data_m, aes(x = 2022, xend = 2019.755, y = 32, yend = 10), size = 0.5) +
  geom_segment(data = rect_data_m, aes(x = 2022, xend = 2019.755, y = 60, yend = 60), size = 0.5) +
  geom_segment(data = rect_data_f, aes(x = 2022, xend = 2019.75, y = 39, yend = 10), size = 0.5) +
  geom_segment(data = rect_data_f, aes(x = 2022, xend = 2019.75, y = 67, yend = 60), size = 0.5) +
  guides(shape = guide_legend(title = "Region", override.aes = list(linetype = 0, 
                                                  color = c("#ef476f", "#FFA500", "#118ab2"), size = 6)),
         linetype = guide_legend(order = 1), fill = "none")

le0_23_24_2

# ---------------------------------------------------------------------------- #
#     3. Save plot
# ---------------------------------------------------------------------------- #

# ggsave(le0_23_24_2, file = "figures/LE_regions_zoom_agesex_pr.pdf", height = 7.32, width = 9.12)
