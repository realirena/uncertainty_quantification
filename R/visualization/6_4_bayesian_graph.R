rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)
library(reshape2)
library(ggpubr)
## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")

dt <- 
  readRDS("data/ex0_noc.rds") %>% 
  mutate(sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))
dt_gaza <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019"& region=="Gaza Strip") %>%
  mutate(lss = ex_noc - ex_cnf, 
         sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))

#gaza_plot <- melt(dt_gaza, id.vars=c("sex", "source", "region", "year"), variable.name="metric", value.name="le0")


dt_palestine <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019"& region=="Palestine") %>%
  mutate(scenario="Counterfactual with\n no conflict deaths", 
         lss = ex_noc - ex_cnf,
                  sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))
  
panels <- tibble(sex = rep(c("Females", "Males", "Total"), 2), 
                 type = c(rep("Life expectancy \nat birth", 3), 
                          rep("Life expectancy \nloss to conflict", 3)),
                 panel = c("A", "B", "C", "D", "E", "F"),
                 ypos = c(rep(76, 3), rep(21, 3)))



### add new graphing code to plot the uncertainty estimates: 
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/gaza/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

oct_26_all <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars])

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts_lifetable_t_le0.csv"))

bts_all <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars])


# UN genocide pattern -----------------------------------------------------

###UN dist results 
un_le_m_geno23 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_le_f_geno23 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_le_t_geno23 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide23_all <- rbind(un_le_f_geno23[,vars], un_le_m_geno23[,vars], un_le_t_geno23[,vars])

le_lss_geno_all <- rbind(oct_26_all, bts_all, un_genocide23_all)

le_lss_geno_all |>
  group_by(sex, year, scenario) |>
  summarise(mean_ex = mean(ex),
    mean_lss = mean(bmmr_lss))

## plot of life expectancies at birth 
le0_plot <- ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = ex_cnf, color="Life Expectancy (observed)"), color="black", alpha = 1, size = 1.5) +
  geom_line(data =dt_gaza, aes(year, ex_noc, linetype = "Life Expectancy (no conflict)"),  alpha = 0.5,
            col = "black", linewidth=1) +
  # counter factual with no conflict deaths
  stat_histinterval(data=le_lss_geno_all, aes(x = year, 
                                         y=ex, group=scenario, 
                                         fill=scenario, 
                                         color=scenario),
                    size=4,
                    alpha=0.5) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  xlab("") + ylab("Life expectancy at birth") + 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1),  shape = "none")+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot <-  ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = lss), color="black", alpha = 0.5, size = 1) +
  stat_histinterval(data=le_lss_geno_all, 
                    aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), 
                    alpha=0.5, size = 4) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  facet_grid(~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+ 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  xlab("Year") + ylab("Life expectancy loss") + 
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# le0_lss <- gridExtra::grid.arrange(le0_plot, lss_plot)

ggarrange(le0_plot, lss_plot, nrow = 2, common.legend = TRUE, legend = "bottom")
# 
#ggsave("figures/un_conflict_pix/gaza_le0_lss_23_unconflict.png", plot=le0_lss, w = 16, h = 8)


### add new graphing code to plot the 2024 uncertainty estimates: 
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2024/gaza/")
## MoH 2024 (oct 23 - oct 24) results
moh_le_m_24 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
moh_le_f_24 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
moh_le_t_24 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

moh_24_all  <- rbind(moh_le_m_24[,vars], moh_le_f_24[,vars],moh_le_t_24[,vars])


bts_le_m_24 <- read.csv(paste0(results_dir, "bts_lifetable_m_le0.csv"))
bts_le_f_24 <- read.csv(paste0(results_dir, "bts_lifetable_f_le0.csv"))
bts_le_t_24 <- read.csv(paste0(results_dir, "bts_lifetable_t_le0.csv"))

bts24_all <- rbind(bts_le_f_24[,vars], bts_le_m_24[,vars], bts_le_t_24[,vars])

un_le_m_genocide24 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_le_f_genocide24  <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_le_t_genocide24  <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide24_all <- rbind(un_le_f_genocide24[,vars], un_le_m_genocide24[,vars],un_le_t_genocide24[,vars])

un_genocide_all <- rbind(un_genocide23_all, un_genocide24_all)
un_genocide_all$type <- "Genocide"

le_lss_geno_all24 <- rbind(moh_24_all, bts24_all, un_genocide24_all)

le0_23_24  <- le0_plot +  stat_histinterval(data=le_lss_geno_all24, aes(x = year, y=ex, group=scenario, 
                                                    fill=scenario, color=scenario),
                               size=4,
                               alpha=0.5) 


lss_23_24  <-  lss_plot +  stat_histinterval(data=le_lss_geno_all24, aes(x = year, y=bmmr_lss, group=scenario, 
                                                    fill=scenario, color=scenario),
                               size=4,
                               alpha=0.5) 

# le0_lss_23_24 <- gridExtra::grid.arrange(le0_23_24, lss_23_24)

le0_lss_23_24 <- ggarrange(le0_23_24, lss_23_24, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(le0_lss_23_24, file = "figures/LE_sources_gaza_genocide.pdf", width = 16, height = 8)

# ggsave("figures/un_conflict_pix/le0_lss_gaza_23_24.png", plot=le0_lss_23_24,
#        w = 16, h = 8)



# UN conflict pattern -----------------------------------------------------
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/gaza/")

un_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_m_le0.csv"))
un_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_f_le0.csv"))
un_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_t_le0.csv"))

un_conf23_all <- rbind(un_le_f_age0[,vars], un_le_m_age0[,vars], un_le_t_age0[,vars])

le_lss_conf_all <- rbind(oct_26_all, bts_all, un_conf23_all)

le_lss_conf_all |>
  group_by(sex, year) |>
  summarise(mean_ex = mean(ex),
            mean_lss = mean(bmmr_lss))


## plot of life expectancies at birth 
le0_plot_conf <- ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = ex_cnf, color="Life Expectancy (observed)"), color="black", alpha = 1, size = 1.5) +
  geom_line(data =dt_gaza, aes(year, ex_noc, linetype = "Life Expectancy (no conflict)"),  alpha = 0.5,
            col = "black", linewidth=1) +
  # counter factual with no conflict deaths
  stat_histinterval(data=le_lss_conf_all, aes(x = year, 
                                              y=ex, group=scenario, 
                                              fill=scenario, 
                                              color=scenario),
                    size=4,
                    alpha=0.5) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  xlab("") + ylab("Life expectancy at birth") + 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1),  shape = "none")+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot_conf <-  ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = lss), color="black", alpha = 0.5, size = 1) +
  stat_histinterval(data=le_lss_conf_all, 
                    aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), 
                    alpha=0.5, size = 4) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  facet_grid(~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+ 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  xlab("Year") + ylab("Life expectancy loss") + 
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# le0_lss <- gridExtra::grid.arrange(le0_plot, lss_plot)

ggarrange(le0_plot_conf, lss_plot_conf, nrow = 2, common.legend = TRUE, legend = "bottom")

## 2024
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2024/gaza/")
un_le_m_conf24 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_m_le0.csv"))
un_le_f_conf24 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_f_le0.csv"))
un_le_t_conf24 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_t_le0.csv"))

un_conf24_all <- rbind(un_le_f_conf24[,vars], un_le_m_conf24[,vars], un_le_t_conf24[,vars])

un_conf_all <- rbind(un_conf23_all, un_conf24_all)
un_conf_all$type <- "Conflict"

le_lss_conf_all24 <- rbind(moh_24_all, bts24_all, un_conf24_all)


le0_conf_23_24 <- le0_plot_conf +  
  stat_histinterval(data=le_lss_conf_all24, aes(x = year, y=ex, group=scenario, 
                                                fill=scenario, color=scenario),
                                                     size=2,
                                                     alpha=0.5) 


lss_conf_23_24  <-  lss_plot_conf + 
  stat_histinterval(data=le_lss_conf_all24, aes(x = year, y=bmmr_lss, group=scenario,
                                                fill=scenario, color=scenario),
                                                       size=2,
                                                       alpha=0.5) 

# le0_lss_conf_23_24 <- gridExtra::grid.arrange(le0_conf_23_24, lss_conf_23_24)

le0_lss_conf_23_24 <- ggarrange(le0_conf_23_24, lss_conf_23_24, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(le0_lss_conf_23_24, file = "figures/LE_sources_gaza_conflict.pdf", width = 16, height = 8)


# UN earthquake pattern ---------------------------------------------------
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/gaza/")
un_le_m_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_m_le0.csv"))
un_le_f_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_f_le0.csv"))
un_le_t_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_t_le0.csv"))

un_earth23_all <- rbind(un_le_f_earth23[,vars], un_le_m_earth23[,vars], un_le_t_earth23[,vars])

le_lss_earth_all <- rbind(oct_26_all, bts_all, un_earth23_all)


## plot of life expectancies at birth 
le0_plot_earth <- ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = ex_cnf, color="Life Expectancy (observed)"), color="black", alpha = 1, size = 1.5) +
  geom_line(data =dt_gaza, aes(year, ex_noc, linetype = "Life Expectancy (no conflict)"),  alpha = 0.5,
            col = "black", linewidth=1) +
  # counter factual with no earthlict deaths
  stat_histinterval(data=le_lss_earth_all, aes(x = year, 
                                              y=ex, group=scenario, 
                                              fill=scenario, 
                                              color=scenario),
                    size=4,
                    alpha=0.5) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  xlab("") + ylab("Life expectancy at birth") + 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1),  shape = "none")+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot_earth <-  ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = lss), color="black", alpha = 0.5, size = 1) +
  stat_histinterval(data=le_lss_earth_all, 
                    aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), 
                    alpha=0.5, size = 4) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  facet_grid(~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+ 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  xlab("Year") + ylab("Life expectancy loss") + 
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# le0_lss_earth_23 <- gridExtra::grid.arrange(le0_plot_earth, lss_plot_earth)
ggarrange(le0_plot_earth, lss_plot_earth, nrow = 2, common.legend = TRUE, legend = "bottom")

### 2024
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2024/gaza/")

un_le_m_earth24 <- read.csv(paste0(results_dir, "un_earth24_lifetable_m_le0.csv"))
un_le_f_earth24 <- read.csv(paste0(results_dir, "un_earth24_lifetable_f_le0.csv"))
un_le_t_earth24 <- read.csv(paste0(results_dir, "un_earth24_lifetable_t_le0.csv"))

un_earth24_all <- rbind(un_le_f_earth24[,vars], un_le_m_earth24[,vars], un_le_t_earth24[,vars])

le_lss_earth_all24 <- rbind(moh_24_all, bts24_all, un_earth24_all)


le0_earth_23_24 <- le0_plot_earth +  
  stat_histinterval(data=le_lss_earth_all24, aes(x = year, y=ex, group=scenario, 
                                                 fill=scenario, color=scenario),
                                                     size=2,
                                                     alpha=0.5) 


lss_earth_23_24  <-  lss_plot_earth +  
  stat_histinterval(data=le_lss_earth_all24, aes(x = year, y=bmmr_lss, group=scenario, 
                                                 fill=scenario, color=scenario),
                                                       size=2,
                                                       alpha=0.5) 

# le0_lss_earth_23_24 <- gridExtra::grid.arrange(le0_earth_23_24, lss_earth_23_24)
# ggsave("figures/un_earth_pix/le0_lss_gaza_23_24_earth.png", plot=le0_lss_earth_23_24, w = 16, h = 8)
le0_lss_earth_23_24 <- ggarrange(le0_earth_23_24, lss_earth_23_24, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(le0_lss_earth_23_24, file = "figures/LE_sources_gaza_earthquake.pdf", width = 16, height = 8)


# ALL UN patterns ---------------------------------------------------
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/gaza/")
un_le_m_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_m_le0.csv"))
un_le_f_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_f_le0.csv"))
un_le_t_earth23 <- read.csv(paste0(results_dir, "un_earth23_lifetable_t_le0.csv"))

un_earth23_all <- rbind(un_le_f_earth23[,vars], un_le_m_earth23[,vars], un_le_t_earth23[,vars])

le_lss_un_all <- rbind(oct_26_all, bts_all, un_earth23_all %>%
                         mutate(scenario = "UN-IGME earthquake pattern"), 
                       un_conf23_all%>%
                         mutate(scenario = "UN-IGME conflict pattern"), 
                       un_genocide23_all %>%
                         mutate(scenario = "UN-IGME genocide pattern"))


## plot of life expectancies at birth 
le0_plot_un <- ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = ex_cnf, color="Life Expectancy (observed)"), color="black", alpha = 1, size = 1.5) +
  geom_line(data =dt_gaza, aes(year, ex_noc, linetype = "Life Expectancy (no conflict)"),  alpha = 0.5,
            col = "black", linewidth=1) +
  # counter factual with no earthlict deaths
  stat_histinterval(data=le_lss_un_all, aes(x = year, 
                                               y=ex, group=scenario, 
                                               fill=scenario, 
                                               color=scenario),
                    size=4,
                    alpha=0.5) +
  scale_color_manual("Scenario",values = c("#07c8ba","#ef476f","#6520c2", "#fec30b","#3aab07"),
                     guide='none') +
  scale_fill_manual(name = "Scenario",values = c("#07c8ba","#ef476f","#6520c2", "#fec30b","#3aab07")) +
  xlab("") + ylab("Life expectancy at birth") + 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(fill = guide_legend(override.aes = list(linetype = 0, pch = NA), ncol = 2),
         linetype = guide_legend(order = 1, ncol = 2))+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot_un <-  ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = lss), color="black", alpha = 0.5, size = 1) +
  stat_histinterval(data=le_lss_un_all, 
                    aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), 
                    alpha=0.5, size = 4) +
  scale_color_manual("Scenario",values = c("#07c8ba","#ef476f","#6520c2", "#fec30b","#3aab07"),
                     guide='none') +
  scale_fill_manual(name = "Scenario",values = c("#07c8ba","#ef476f","#6520c2", "#fec30b","#3aab07")) +
  facet_grid(~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+ 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  xlab("Year") + ylab("Life expectancy loss") + 
  guides(fill = guide_legend(override.aes = list(linetype = 0, pch = NA), ncol = 2),
         linetype = guide_legend(order = 1, ncol = 2))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# le0_lss_earth_23 <- gridExtra::grid.arrange(le0_plot_earth, lss_plot_earth)
ggarrange(le0_plot_un, lss_plot_un, nrow = 2, common.legend = TRUE, legend = "bottom")

### 2024
le_lss_un_all24 <- rbind(moh_24_all, bts24_all, un_earth24_all %>%
                         mutate(scenario = "UN-IGME earthquake pattern"), 
                       un_conf24_all%>%
                         mutate(scenario = "UN-IGME conflict pattern"), 
                       un_genocide24_all %>%
                         mutate(scenario = "UN-IGME genocide pattern"))


le0_un_23_24 <- le0_plot_un +  
  stat_histinterval(data=le_lss_un_all24, aes(x = year, y=ex, group=scenario, 
                                                 fill=scenario, color=scenario),
                    size=2,
                    alpha=0.5) 


lss_un_23_24  <-  lss_plot_un +  
  stat_histinterval(data=le_lss_un_all24, aes(x = year, y=bmmr_lss, group=scenario, 
                                                 fill=scenario, color=scenario),
                    size=2,
                    alpha=0.5) 

# le0_lss_earth_23_24 <- gridExtra::grid.arrange(le0_earth_23_24, lss_earth_23_24)
# ggsave("figures/un_earth_pix/le0_lss_gaza_23_24_earth.png", plot=le0_lss_earth_23_24, w = 16, h = 8)
le0_lss_un_23_24 <- ggarrange(le0_un_23_24, lss_un_23_24, nrow = 2, common.legend = TRUE, legend = "bottom")

ggsave(le0_lss_un_23_24, file = "figures/LE_sources_gaza_all_scenarios.pdf", width = 16, height = 8)




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


un_earth24_all <- rbind(un_le_f_earth24[,vars], un_le_m_earth24[,vars], un_le_t_earth24[,vars])
un_earth_all <- rbind(un_earth23_all, un_earth24_all)
un_earth_all$type <- "Earth"


un_all <- rbind(un_conflict_all, un_genocide_all, un_earth_all)

## plot of life expectancies at birth 
le0_plot <- ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = ex_cnf), color="black", alpha = 1, size = 1.5) + 
  geom_line(data = dt_gaza %>% filter(year <= 2022), aes(year, ex_noc, linetype = "Life Expectancy (no conflict)"), 
            col = "black", linewidth=1) + 
  # counter factual with no conflict deaths
  geom_point(data = dt_gaza %>% filter(year > 2022), 
             aes(x = year, y = ex_noc), color="grey50", group = "Life Expectancy (no conflict)", alpha = 1, size = 1.5)+
  stat_histinterval(data=un_all, aes(x = year, y=ex, group=type, 
                                         fill=type, color=type),
                    size=2,
                    alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2")) + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none') +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
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
  geom_point(data = dt_gaza  %>% filter(year <= 2022), 
             aes(x = year, y = lss), color="black", alpha = 1, size = 1) +   
  stat_histinterval(data=un_all, aes(x = year, y=bmmr_lss, group=type, fill=type, color=type), alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  # scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  scale_color_manual("", values = c("#ef476f", "#FFA500", "#118ab2")) + 
  scale_fill_manual(guide="none", values = c("#ef476f", "#FFA500", "#118ab2")) +
  facet_grid(~ sex, scale = "free_y", space = "free_y", switch = "y") +
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



un_plots <- gridExtra::grid.arrange(le0_plot,  lss_plot)
ggsave("figures/le0_lss_gaza_un_only.png", plot=un_plots, w = 16, h = 8)
