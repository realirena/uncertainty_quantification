rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)
## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")

dt <- 
  read_csv("data_inter/ex_0_all_scenarios.csv") %>% 
  mutate(sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))

## data for all regions 
dt_ex <- 
  readRDS("data_inter/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019") %>%
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
results_dir <- paste0(getwd(),"/R/bmmr/samples/2023/pcbs/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## Palestine results 
nat_le_m_age0 <- read.csv(paste0(results_dir, "2019/palestine/moh_lifetable_m_le0.csv"))
nat_le_f_age0 <- read.csv(paste0(results_dir, "2019/palestine/moh_lifetable_f_le0.csv"))
nat_le_t_age0 <- read.csv(paste0(results_dir, "2019/palestine/moh_lifetable_t_le0.csv"))

nat_all <- rbind(nat_le_f_age0[,vars], nat_le_m_age0[,vars], nat_le_t_age0[,vars])

## Gaza results
gaza_le_m_age0 <- read.csv(paste0(results_dir, "2019/gaza/moh_lifetable_m_le0.csv"))
gaza_le_f_age0 <- read.csv(paste0(results_dir, "2019/gaza/moh_lifetable_f_le0.csv"))
gaza_le_t_age0 <- read.csv(paste0(results_dir, "2019/gaza/moh_lifetable_t_le0.csv"))

gaza_all <- rbind(gaza_le_f_age0[,vars], gaza_le_m_age0[,vars], gaza_le_t_age0[,vars])

### West Bank estimates -> replace w/ file names of the west bank lifetables 
wb_le_m_age0 <- read.csv(paste0(results_dir, "wb_lifetable_m_le0.csv"))
wb_le_f_age0 <- read.csv(paste0(results_dir, "wb_lifetable_f_le0.csv"))
wb_le_t_age0 <- read.csv(paste0(results_dir, "wb_lifetable_t_le0.csv"))

wb_all <- rbind(wb_le_f_age0[,vars], wb_le_m_age0[,vars], wb_le_t_age0[,vars])

le_lss_all <- rbind(nat_all, gaza_all, wb_all)

## plot of life expectancies at birth 
le0_plot <- ggplot() +   
  geom_point(data = dt_ex %>% filter(year <= 2022),
             aes(x = year, y = ex_cnf,color=region,),  alpha = 1, size = 1.5) + 
  geom_line(data = dt_ex, aes(year, ex_noc, color=region, group=region, linetype = "Life Expectancy (no conflict)"),alpha=0.5,  linewidth=1) + 
  # counter factual with no conflict deaths
  # geom_line(data = dt_ex %>% filter(year > 2022), 
  #            aes(x = year, y = ex_noc, color=region),  group = "Life Expectancy (no conflict)", alpha = 0.5, size = 1.5)+
  stat_histinterval(data=le_lss_all, aes(x = year, y=ex, group=region, 
                                         fill=region, color=region),
                    size=2,
                    alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Gaza Strip",
                       "Palestine",
                       "West Bank")) + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none'
                    # ,labels = c(
                    #  "GMoH", 
                    #  "Historical\naverage",
                    #  "UN-IGME")
  ) +
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

## plot of life expectancy loss
lss_plot <-  ggplot() +   
  geom_point(data = dt_ex  %>% filter(year <= 2022), 
             aes(x = year, y = lss, color=region), alpha = 1, size = 1) +   
  stat_histinterval(data=le_lss_all, aes(x = year, y=bmmr_lss, group=region, fill=region, color=region), alpha=0.5) +
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  # scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
  scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Gaza Strip",
                       "Palestine",
                       "West Bank")) + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"),guide = 'none'
                    # ,labels = c(
                    #  "GMoH", 
                    #  "Historical\naverage",
                    #  "UN-IGME")
  ) +
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

le0_lss <- gridExtra::grid.arrange(le0_plot, lss_plot)
# 
# ggsave("figures/un_conflict_pix/le0_lss_palestine_23.png", plot=le0_lss,    
#        w = 16, h = 8)

#### ---------------
## 2024 results
#### ---------------

### add new graphing code to plot the 2024 uncertainty estimates: 
results_dir <- paste0(getwd(),"/R/bmmr/samples/2024/pcbs/")

## palestine 

nat_le_m_24 <- read.csv(paste0(results_dir, "2019/palestine/moh_lifetable_m_le0.csv"))
nat_le_f_24 <- read.csv(paste0(results_dir, "2019/palestine/moh_lifetable_f_le0.csv"))
nat_le_t_24 <- read.csv(paste0(results_dir, "2019/palestine/moh_lifetable_t_le0.csv"))

nat_24_all  <- rbind(nat_le_m_24[,vars], nat_le_f_24[,vars],nat_le_t_24[,vars])
nat_24_all$region="Palestine"

## gaza 
gaza_le_m_24 <- read.csv(paste0(results_dir, "2019/gaza/moh_lifetable_m_le0.csv"))
gaza_le_f_24 <- read.csv(paste0(results_dir, "2019/gaza/moh_lifetable_f_le0.csv"))
gaza_le_t_24 <- read.csv(paste0(results_dir, "2019/gaza/moh_lifetable_t_le0.csv"))

gaza_24_all <- rbind(gaza_le_f_24[,vars], gaza_le_m_24[,vars], gaza_le_t_24[,vars])
gaza_24_all$region="Gaza Strip"

## replace with names of the WB lifetables 
wb_f_24 <- read.csv(paste0(results_dir, "wb_lifetable_m_le0.csv"))
wb_m_24   <- read.csv(paste0(results_dir, "wb_lifetable_f_le0.csv"))
wb_t_24  <- read.csv(paste0(results_dir, "wb_lifetable_t_le0.csv"))

wb_24_all <- rbind(wb_f_24[,vars], wb_m_24[,vars],wb_t_24[,vars])
wb_24_all$region <- "West Bank"

le_lss_all24 <- rbind(nat_24_all, gaza_24_all, wb_24_all)


## add 2024 to the plots 
le0_23_24  <- le0_plot +  stat_histinterval(data=le_lss_all24, aes(x = year, y=ex, group=region, 
                                                                   fill=region, color=region),
                               size=2,
                               alpha=0.5) 


lss_23_24  <-  lss_plot +  stat_histinterval(data=le_lss_all24, aes(x = year, y=bmmr_lss, group=region, 
                                                    fill=region, color=region),
                               size=2,
                               alpha=0.5) 

le0_lss_23_24 <- gridExtra::grid.arrange(le0_23_24, lss_23_24)

#ggsave("figures/le0_lss_palestine_23_24.png", plot=le0_lss_23_24, w = 16, h = 8)

