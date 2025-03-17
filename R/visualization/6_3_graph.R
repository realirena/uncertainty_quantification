rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)

dt <- 
  read_csv("data_inter/ex_0_all_scenarios.csv") %>% 
  mutate(sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))

dt2 <- 
  dt %>% 
  filter(coverage %in% c(0.5, 0.8, 1, 1.2, 1.5),
         scenario != "WPP") %>% 
  mutate(scenario = case_when(scenario == "Conflict 2023 (Oct 26. age dist.)" ~ "GMoH",
                              scenario == "UN" ~ "UN-IGME",
                              scenario == "No conflict deaths" ~ "Counterfactual",
                              TRUE ~ "B'Tselem")) %>% 
  select(-1, -ex_ul, -ex_ll, -mx, -age, -country) %>% 
  filter(!(scenario != "Counterfactual" & year != 2023)) %>% 
  spread(coverage, ex) %>% 
  rename(ex_l1 = `0.5`,
         ex_l2 = `0.8`,
         ex = `1`,
         ex_u1 = `1.2`,
         ex_u2 = `1.5`) %>% 
  mutate(type = "Life expectancy \nat birth",
         year = case_when(scenario == "GMoH" ~ 2022.5,
                          scenario == "UN-IGME" ~ 2023,
                          scenario == "B'Tselem" ~ 2023.5,
                          TRUE ~ year),
         scenario = factor(scenario, levels = c("Counterfactual", 
                                                "GMoH", 
                                                "UN-IGME",
                                                "B'Tselem")))

wpp <- 
  dt %>% 
  filter(scenario == "WPP",
         year != 2023) %>% 
  select(-1, -mx, -ex_ul, -ex_ll, -coverage, -scenario, -age, -country) %>% 
  mutate(type = "Life expectancy \nat birth")

lss <-
  wpp %>% 
  mutate(scenario = "WPP") %>% 
  bind_rows(dt2 %>% 
              filter(scenario != "Counterfactual") %>% 
              mutate(year = 2023)) %>% 
  left_join(dt2 %>% 
              filter(scenario == "Counterfactual") %>% 
              rename(ex_ncnf = ex) %>% 
              select(-ex_l1, -ex_u1, -ex_l2, -ex_u2, -scenario)) %>% 
  mutate(ex_loss = ex_ncnf - ex,
         ex_loss_l1 = ex_ncnf- ex_l1,
         ex_loss_l2 = ex_ncnf- ex_l2,
         ex_loss_u1 = ex_ncnf - ex_u1,
         ex_loss_u2 = ex_ncnf - ex_u2) %>% 
  mutate(type = "Life expectancy \nloss to conflict",
         year = case_when(scenario == "GMoH" ~ 2022.5,
                          scenario == "UN-IGME" ~ 2023,
                          scenario == "B'Tselem" ~ 2023.5,
                          TRUE ~ year)) %>% 
  select(-ex_l1, -ex_u1, -ex_l2, -ex_u2, ex)

# plot
panels <- tibble(sex = rep(c("Females", "Males", "Total"), 2), 
                 type = c(rep("Life expectancy \nat birth", 3), 
                          rep("Life expectancy \nloss to conflict", 3)),
                 panel = c("A", "B", "C", "D", "E", "F"),
                 ypos = c(rep(76, 3), rep(21, 3)))

# plot with bars
ggplot()+
  facet_grid(type ~ sex, scale = "free_y", space = "free_y",
             switch = "y")+
  geom_point(data = dt2 %>% filter(year > 2022),
               aes(x = year, y = ex,
                   col = scenario),
               size = 1.3, na.rm = TRUE)+
  # ex ranges
  geom_segment(data = dt2 %>% filter(year > 2022),
               aes(x = year, xend = year, y = ex_l1, yend = ex_u2,
                   col = scenario),
               size = 0.3, na.rm = TRUE)+
  geom_point(data = dt2 %>% filter(year > 2022),
             aes(x = year, y = ex_l1,
                 col = scenario),
             size = 2.5, shape = 95, na.rm = TRUE)+
  geom_point(data = dt2 %>% filter(year > 2022),
             aes(x = year, y = ex_l2,
                 col = scenario),
             size = 2, shape = 95, na.rm = TRUE)+
  geom_point(data = dt2 %>% filter(year > 2022),
             aes(x = year, y = ex_u1,
                 col = scenario),
             size = 2, shape = 95, na.rm = TRUE)+
  geom_point(data = dt2 %>% filter(year > 2022),
             aes(x = year, y = ex_u2,
                 col = scenario),
             size = 2.5, shape = 95, na.rm = TRUE)+
  # life expectancy WPP
  geom_line(data = wpp, aes(year, ex, linetype = "Observed life\nexpectancy in WPP"), col = "black")+
  # counter factual with no conflict deaths
  geom_point(data = dt2 %>% filter(year <= 2022, scenario == "Counterfactual"), 
             aes(x = year, y = ex), 
             color="grey50", alpha = 1, size = 1)+
  # life expectancy losses 2000-2022
  geom_point(data = lss %>% filter(year <= 2022), 
             aes(x = year, y = ex_loss), 
             color="grey50", alpha = 1, size = 1)+
  # life expectancy losses 2023
  geom_point(data = lss %>% filter(year > 2022), 
             aes(x = year, y = ex_loss,
                 col = scenario), alpha = 1, size = 1)+
  geom_segment(data = lss %>% filter(year > 2022),
               aes(x = year, xend = year, y = ex_loss_l1, yend = ex_loss_u2,
                   col = scenario),
               size = 0.3, na.rm = TRUE)+
  geom_point(data = lss %>% filter(year > 2022),
             aes(x = year, y = ex_loss_l1,
                 col = scenario),
             size = 2, shape = 95, na.rm = TRUE)+
  geom_point(data = lss %>% filter(year > 2022),
             aes(x = year, y = ex_loss_l2,
                 col = scenario),
             size = 1.5, shape = 95, na.rm = TRUE)+
  geom_point(data = lss %>% filter(year > 2022),
             aes(x = year, y = ex_loss_u1,
                 col = scenario),
             size = 1.5, shape = 95, na.rm = TRUE)+
  geom_point(data = lss %>% filter(year > 2022),
             aes(x = year, y = ex_loss_u2,
                 col = scenario),
             size = 2, shape = 95, na.rm = TRUE)+
  geom_text(data = panels, aes(2000, ypos, label = panel), fontface = "bold")+
  scale_linetype_discrete(name="")+
  scale_color_manual(values = c("grey50", "#ef476f", "#118ab2", "#06d6a0"),
                     labels = c("Counterfactual with\n no conflict deaths", 
                                "GMoH", 
                                "UN-IGME",
                                "Historical\naverage"))+
  scale_x_continuous(breaks = c(seq(2000, 2020, 5), 2023))+
  scale_y_continuous(breaks = c(seq(-20, 80, 5), seq(4000, 24000, 6000)))+
  labs(color = "Life expectancy\nscenario", x = "Year")+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

# ggsave("figures/fig1.png",
#        w = 12, h = 5)



### add new graphing code to plot the uncertainty estimates: 
results_dir <- paste0(getwd(),"/R/bmmr/samples/le_estimates/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh_oct_26_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh_oct_26_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh_oct_26_t_le0.csv"))

oct_26_all <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars])

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts23_bayes_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts23_bayes_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts23_bayes_lifetable_t_le0.csv"))

bts_all <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars])

###UN dist results 
un_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_m_le0.csv"))
un_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_f_le0.csv"))
un_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_t_le0.csv"))

un_all <- rbind(un_le_f_age0[,vars], un_le_m_age0[,vars], un_le_t_age0[,vars])

le_lss_all <- rbind(oct_26_all, bts_all, un_all)

## plot of life expectancies at birth 
le0_plot <- ggplot() +   
  geom_point(data = dt2 %>% filter(year <= 2022, scenario == "Counterfactual"),
             aes(x = year, y = ex), color="black", alpha = 1, size = 1.5) + 
  geom_line(data = wpp, aes(year, ex, linetype = "Observed life\nexpectancy in WPP"), 
            col = "black", linewidth=1) + 
  # counter factual with no conflict deaths
  geom_point(data = dt2 %>% filter(year > 2022, scenario == "Counterfactual"), 
             aes(x = year, y = ex,color=scenario), alpha = 1, size = 1.5)+
  stat_histinterval(data=le_lss_all, aes(x = year, y=ex, group=scenario, 
                                         fill=scenario, color=scenario),
                    size=2,
                    alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  scale_color_manual("",values = c("grey50","#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Counterfactual with\n no conflict deaths", 
                       "GMoH",
                       "Historical\naverage",
                       "UN-IGME")) + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none'
                    # ,labels = c(
                    #  "GMoH", 
                    #  "Historical\naverage",
                    #  "UN-IGME")
  ) +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot <-  ggplot() +   
  geom_point(data = lss %>% filter(year <= 2022), 
             aes(x = year, y = ex_loss), color="black", alpha = 1, size = 1) +   
  stat_histinterval(data=le_lss_all, aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  # scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  scale_color_manual("", values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "GMoH", 
                       "Historical\naverage",
                       "UN-IGME")) + 
  scale_fill_manual(guide="none", values = c("#ef476f", "#FFA500", "#118ab2"),
                    labels = c( 
                      "GMoH", 
                      "Historical\naverage",
                      "UN-IGME")) +
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

le0_lss <- gridExtra::grid.arrange(le0_plot, lss_plot)
# 
# ggsave("figures/un_conflict_pix/le0_lss_palestine_23.png", plot=le0_lss,    
#        w = 16, h = 8)


### add new graphing code to plot the 2024 uncertainty estimates: 

## MoH 2024 (oct 23 - oct 24) results
moh_le_m_24 <- read.csv(paste0(results_dir, "moh_24_m_le0.csv"))
moh_le_f_24 <- read.csv(paste0(results_dir, "moh_24_f_le0.csv"))
moh_le_t_24 <- read.csv(paste0(results_dir, "moh_24_t_le0.csv"))

moh_24_all  <- rbind(moh_le_m_24[,vars], moh_le_f_24[,vars],moh_le_t_24[,vars])


bts_le_m_24 <- read.csv(paste0(results_dir, "bts24_bayes_lifetable_m_le0.csv"))
bts_le_f_24 <- read.csv(paste0(results_dir, "bts24_bayes_lifetable_f_le0.csv"))
bts_le_t_24 <- read.csv(paste0(results_dir, "bts24_bayes_lifetable_t_le0.csv"))

bts24_all <- rbind(bts_le_f_24[,vars], bts_le_m_24[,vars], bts_le_t_24[,vars])

un_le_m_conflict24 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_m_le0.csv"))
un_le_f_conflict24  <- read.csv(paste0(results_dir, "un_conflict24_lifetable_f_le0.csv"))
un_le_t_conflict24  <- read.csv(paste0(results_dir, "un_conflict24_lifetable_t_le0.csv"))

un_conflict24_all <- rbind(un_le_f_conflict24[,vars], un_le_m_conflict24[,vars],un_le_t_conflict24[,vars])

le_lss_all24 <- rbind(moh_24_all, bts24_all, un_conflict24_all)


le0_23_24  <- le0_plot +  stat_histinterval(data=le_lss_all24, aes(x = year, y=ex, group=scenario, 
                                                    fill=scenario, color=scenario),
                               size=2,
                               alpha=0.5) 


lss_23_24  <-  lss_plot +  stat_histinterval(data=le_lss_all24, aes(x = year, y=bmmr_lss, group=scenario, 
                                                    fill=scenario, color=scenario),
                               size=2,
                               alpha=0.5) 

le0_lss_23_24 <- gridExtra::grid.arrange(le0_23_24, lss_23_24)

ggsave("figures/un_conflict_pix/wider_reporting/le0_lss_palestine_23_24.png", plot=le0_lss_23_24,
       w = 16, h = 8)

##### --------------------------
###UN genocide results 
un_le_m_geno23 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_le_f_geno23 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_le_t_geno23 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide23_all <- rbind(un_le_f_geno23[,vars], un_le_m_geno23[,vars], un_le_t_geno23[,vars])

le_lss_geno_all <- rbind(oct_26_all, bts_all, un_genocide23_all)


## plot of life expectancies at birth 
le0_plot_geno <- ggplot() +   
  geom_point(data = dt2 %>% filter(year <= 2022, scenario == "Counterfactual"),
             aes(x = year, y = ex), color="black", alpha = 1, size = 1.5) + 
  geom_line(data = wpp, aes(year, ex, linetype = "Observed life\nexpectancy in WPP"), 
            col = "black", linewidth=1) + 
  # counter factual with no conflict deaths
  geom_point(data = dt2 %>% filter(year > 2022, scenario == "Counterfactual"), 
             aes(x = year, y = ex,color=scenario), alpha = 1, size = 1.5)+
  stat_histinterval(data=le_lss_geno_all, aes(x = year, y=ex, group=scenario, 
                                         fill=scenario, color=scenario),
                    size=2,
                    alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  scale_color_manual("",values = c("grey50","#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Counterfactual with\n no conflict deaths", 
                       "GMoH",
                       "Historical\naverage",
                       "UN-IGME")) + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none'
                    # ,labels = c(
                    #  "GMoH", 
                    #  "Historical\naverage",
                    #  "UN-IGME")
  ) +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot_geno <-  ggplot() +   
  geom_point(data = lss %>% filter(year <= 2022), 
             aes(x = year, y = ex_loss), color="black", alpha = 1, size = 1) +   
  stat_histinterval(data=le_lss_geno_all, aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  # scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  scale_color_manual("", values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "GMoH", 
                       "Historical\naverage",
                       "UN-IGME")) + 
  scale_fill_manual(guide="none", values = c("#ef476f", "#FFA500", "#118ab2"),
                    labels = c( 
                      "GMoH", 
                      "Historical\naverage",
                      "UN-IGME")) +
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

le0_lss_geno_23 <- gridExtra::grid.arrange(le0_plot_geno, lss_plot_geno)

ggsave("figures/un_genocide_pix/le0_lss_palestine_23.png", plot=le0_lss_geno_23, w = 16, h = 8)

##### --------------------------
###UN genocide results 
un_le_m_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_m_le0.csv"))
un_le_f_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_f_le0.csv"))
un_le_t_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_t_le0.csv"))

un_earth23_all <- rbind(un_le_f_earth23[,vars], un_le_m_earth23[,vars], un_le_t_earth23[,vars])

le_lss_earth_all <- rbind(oct_26_all, bts_all, un_earth23_all)


## plot of life expectancies at birth 
le0_plot_earth <- ggplot() +   
  geom_point(data = dt2 %>% filter(year <= 2022, scenario == "Counterfactual"),
             aes(x = year, y = ex), color="black", alpha = 1, size = 1.5) + 
  geom_line(data = wpp, aes(year, ex, linetype = "Observed life\nexpectancy in WPP"), 
            col = "black", linewidth=1) + 
  # counter factual with no conflict deaths
  geom_point(data = dt2 %>% filter(year > 2022, scenario == "Counterfactual"), 
             aes(x = year, y = ex,color=scenario), alpha = 1, size = 1.5)+
  stat_histinterval(data=le_lss_earth_all, aes(x = year, y=ex, group=scenario, 
                                              fill=scenario, color=scenario),
                    size=2,
                    alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  scale_color_manual("",values = c("grey50","#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Counterfactual with\n no conflict deaths", 
                       "GMoH",
                       "Historical\naverage",
                       "UN-IGME")) + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none'
                    # ,labels = c(
                    #  "GMoH", 
                    #  "Historical\naverage",
                    #  "UN-IGME")
  ) +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot_earth <-  ggplot() +   
  geom_point(data = lss %>% filter(year <= 2022), 
             aes(x = year, y = ex_loss), color="black", alpha = 1, size = 1) +   
  stat_histinterval(data=le_lss_earth_all, aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  # scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  scale_color_manual("", values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "GMoH", 
                       "Historical\naverage",
                       "UN-IGME")) + 
  scale_fill_manual(guide="none", values = c("#ef476f", "#FFA500", "#118ab2"),
                    labels = c( 
                      "GMoH", 
                      "Historical\naverage",
                      "UN-IGME")) +
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

le0_lss_earth_23 <- gridExtra::grid.arrange(le0_plot_earth, lss_plot_earth)

ggsave("figures/un_earth_pix/le0_lss_palestine_23.png", plot=le0_lss_earth_23, w = 16, h = 8)




un_le_m_geno24 <- read.csv(paste0(results_dir, "un_genocide24_lifetable_m_le0.csv"))
un_le_f_geno24 <- read.csv(paste0(results_dir, "un_genocide24_lifetable_f_le0.csv"))
un_le_t_geno24 <- read.csv(paste0(results_dir, "un_genocide24_lifetable_t_le0.csv"))

un_genocide24_all <- rbind(un_le_f_geno24[,vars], un_le_m_geno24[,vars], un_le_t_geno24[,vars])


le_lss_geno_all24 <- rbind(moh_24_all, bts24_all, un_genocide24_all)



le0_geno_23_24 <- le0_plot_geno +  stat_histinterval(data=le_lss_geno_all24, aes(x = year, y=ex, group=scenario, 
                                                                  fill=scenario, color=scenario),
                                            size=2,
                                            alpha=0.5) 


lss_geno_23_24  <-  lss_plot_geno +  stat_histinterval(data=le_lss_geno_all24, aes(x = year, y=bmmr_lss, group=scenario, 
                                                                   fill=scenario, color=scenario),
                                             size=2,
                                             alpha=0.5) 

le0_lss_geno_23_24 <- gridExtra::grid.arrange(le0_geno_23_24, lss_geno_23_24)

ggsave("figures/un_genocide_pix/le0_lss_palestine_23_24.png", plot=le0_lss_geno_23_24,
       w = 16, h = 8)



## plot of life expectancies at birth 
un_plot1 <- ggplot() +   
  geom_point(data = dt2 %>% filter(year <= 2022, scenario == "Counterfactual"),
             aes(x = year, y = ex), color="black", alpha = 1, size = 1.5) + 
  geom_line(data = wpp, aes(year, ex, linetype = "Observed life\nexpectancy in WPP"), 
            col = "black", linewidth=1) + 
  # counter factual with no conflict deaths
  geom_point(data = dt2 %>% filter(year > 2022, scenario == "Counterfactual"), 
             aes(x = year, y = ex,color=scenario), alpha = 1, size = 1.5)+
  stat_histinterval(data=un_all, aes(x = year, y=ex, group=scenario, 
                                     fill=scenario, color=scenario),
                    size=2,
                    alpha=0.5) +
  scale_color_manual("",values = c("grey50", "#118ab2"),
                     labels = c( 
                       "Counterfactual with\n no conflict deaths", 
                       "UN-IGME")) + 
  scale_fill_manual(values = c("#118ab2"),guide = 'none'
  ) +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+ 
  labs(title="LE Estimates with Reporting and Age Distribution Uncertainty") + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))

## plot of life expectancies at birth 
un_plot2 <- ggplot() +   
  geom_point(data = dt2 %>% filter(year <= 2022, scenario == "Counterfactual"),
             aes(x = year, y = ex), color="black", alpha = 1, size = 1.5) + 
  geom_line(data = wpp, aes(year, ex, linetype = "Observed life\nexpectancy in WPP"), 
            col = "black", linewidth=1) + 
  # counter factual with no conflict deaths
  geom_point(data = dt2 %>% filter(year > 2022, scenario == "Counterfactual"), 
             aes(x = year, y = ex,color=scenario), alpha = 1, size = 1.5)+
  stat_histinterval(data=un_genocide_all, aes(x = year, y=ex, group=scenario, 
                                     fill=scenario, color=scenario),
                    size=2,
                    alpha=0.5) +
  scale_color_manual("",values = c("grey50", "#118ab2"),
                     labels = c( 
                       "Counterfactual with\n no conflict deaths", 
                       "UN-IGME")) + 
  scale_fill_manual(values = c("#118ab2"),guide = 'none'
  ) +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  facet_grid(type ~ sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  labs(title="LE Estimates with only Age Distribution Uncertainty") + 
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))


gridExtra::grid.arrange(un_plot1, un_plot2)
