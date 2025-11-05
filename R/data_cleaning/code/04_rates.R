rm(list=ls())
library(tidyverse)
library(readxl)
library(data.table)

pop <- read_rds("R/data_cleaning/data_inter/pcbs_exposures_region_interpol_open_age80.rds")
dts <- read_rds("R/data_cleaning/data_inter/vr_deaths_regions_palestine_2012_2023.rds")
# Gaza 2017-2022
qs_gz <- read_rds("R/data_cleaning/data_inter/infant_child_mortality_rates_igme.rds")

qs_gz2 <- 
  qs_gz %>% 
  bind_rows(
    qs_gz %>% 
      filter(year == 2022) %>% 
      mutate(year = 2023)
  )

unique(pop$age)
unique(dts$age)

pop2 <- 
  pop %>% 
  mutate(age = case_when(age %in% 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age %in% 5:79 ~ age-age%%5,
                         age >= 80 ~ 80)) %>% 
  reframe(pop = sum(pop),
          .by = c(region, year, sex, age))

mx_pcbs <- 
  pop2 %>% 
  filter(year >= 2012) %>% 
  left_join(dts %>% 
              filter(age >= 5)) %>% 
  filter(sex != "t") %>% 
  left_join(qs_gz2) %>% 
  mutate(dx = ifelse(age <= 1, pop*mx, dx),
         mx = ifelse(age >= 5, 1e5*dx/pop, mx*1e5),
         source = "pcbs")
  
mx_pcbs2 <- 
  mx_pcbs %>% 
  bind_rows(mx_pcbs %>% 
              reframe(dx = sum(dx),
                      pop = sum(pop),
                      .by = c(region, year, age, source)) %>% 
              mutate(mx = 1e5*dx/pop,
                     sex = "t")) %>% 
  mutate(dx = ifelse(region != "West Bank" & year == 2023, NA, dx),
         mx = ifelse(region != "West Bank" & year == 2023, NA, mx))

# 
# mx_pcbs <- 
#   dts %>% 
#   left_join(pop2) %>% 
#   mutate(mx = 1e5*dx/pop,
#          source = "pcbs")

gz_pop_fr <- 
  mx_pcbs2 %>% 
  select(-dx, -mx) %>% 
  spread(region, pop) %>% 
  mutate(gz_pop_fr = `Gaza Strip`/Palestine) %>% 
  select(year, sex, age, gz_pop_fr)

gz_dth_fr <- 
  mx_pcbs2 %>% 
  select(-pop, -mx) %>% 
  spread(region, dx) %>% 
  mutate(gz_dth_fr = `Gaza Strip`/Palestine) %>% 
  select(year, sex, age, gz_dth_fr)

pop_wpp <- 
  fread(file.path("R/data_cleaning/data_input", 
                  "WPP2024_PopulationBySingleAgeSex_Medium_1950-2023.csv.gz"))

pop_wpp_pr <- 
  fread(file.path("R/data_cleaning/data_input", 
                  "WPP2024_PopulationBySingleAgeSex_Medium_2024-2100.csv.gz"))

pop_wpp2 <- 
  pop_wpp %>% 
  rename_with(tolower) %>% 
  select(iso = iso3_code, 
         year = time, age = agegrp, m = popmale, f = popfemale, t = poptotal) %>% 
  filter(iso == "PSE") %>% 
  as_tibble() %>% 
  filter(year %in% 2012:2023)

pop_wpp_pr2 <- 
  pop_wpp_pr %>% 
  rename_with(tolower) %>% 
  select(iso = iso3_code, 
         year = time, age = agegrp, m = popmale, f = popfemale, t = poptotal) %>% 
  filter(iso == "PSE") %>% 
  as_tibble() %>% 
  filter(year %in% 2024)

pop_wpp3 <- 
  bind_rows(pop_wpp2,
            pop_wpp_pr2) %>% 
  gather(m, f, t, key = sex, value = pop) %>% 
  mutate(pop = as.double(pop) * 1000, 
         age = ifelse(age == "100+", 100, as.double(age)),
         age = case_when(age %in% 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age %in% 5:79 ~ age-age%%5,
                         age >= 80 ~ 80)) %>% 
  reframe(pop = sum(pop),
          .by = c(year, sex, age)) %>% 
  left_join(gz_pop_fr) %>% 
  mutate(pop_gza = pop * gz_pop_fr,
         pop_wbk = pop - pop_gza) %>% 
  select(-gz_pop_fr) %>% 
  gather(starts_with("pop"), key = region, value = pop) %>% 
  mutate(region = case_when(region == "pop" ~ "Palestine",
                            region == "pop_gza" ~ "Gaza Strip",
                            region == "pop_wbk" ~ "West Bank"))

dts_wpp <- 
  fread(file.path("R/data_cleaning/data_input", 
                  "WPP2024_DeathsBySingleAgeSex_Medium_1950-2023.csv.gz"))

dts_wpp2 <- 
  dts_wpp %>% 
  rename_with(tolower) %>% 
  select(iso = iso3_code, 
         year = time, age = agegrp, m = deathmale, f = deathfemale, t = deathtotal) %>% 
  filter(iso == "PSE") %>% 
  as_tibble() %>% 
  filter(year %in% 2012:2023) %>% 
  gather(m, f, t, key = sex, value = dx) %>% 
  mutate(dx = as.double(dx) * 1000, 
         age = ifelse(age == "100+", 100, as.double(age)),
         age = case_when(age %in% 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age %in% 5:79 ~ age-age%%5,
                         age >= 80 ~ 80)) %>% 
  reframe(dx = sum(dx),
          .by = c(year, sex, age)) %>% 
  left_join(gz_dth_fr) %>% 
  mutate(dx_gza = dx * gz_dth_fr,
         dx_wbk = dx - dx_gza) %>% 
  select(-gz_dth_fr) %>% 
  gather(starts_with("dx"), key = region, value = dx) %>% 
  mutate(region = case_when(region == "dx" ~ "Palestine",
                            region == "dx_gza" ~ "Gaza Strip",
                            region == "dx_wbk" ~ "West Bank"))

mx_wpp <- 
  pop_wpp3 %>% 
  left_join(dts_wpp2) %>% 
  mutate(mx = 1e5*dx/pop,
         source = "wpp2024")
  
mx <- 
  bind_rows(mx_pcbs2,
            mx_wpp)

mx %>% 
  filter(year %in% c(2012, 2016, 2020),
         sex == "t") %>% 
  ggplot()+
  geom_line(aes(age, mx, col = source))+
  scale_y_log10()+
  facet_grid(region~year)+
  theme_bw()

mx %>% 
  filter(year %in% c(2012, 2014, 2018, 2022),
         sex == "t") %>% 
  ggplot()+
  geom_line(aes(age, mx, col = region))+
  scale_y_log10()+
  facet_grid(source~year)+
  theme_bw()


# conflict deaths from 
bts <- 
  read_xlsx("R/data_cleaning/data_input/palest_by_isr_forces.xlsx") %>% 
  select(region = `Event location - Region`,
         date = `Date of event`,
         age = Age,
         sex = Gender) %>% 
  mutate(year = year(date),
         sex = str_to_lower(sex),
         age = case_when(age %in% 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age >= 80 ~ 80,
                         TRUE ~ age - age%%5)) %>% 
  group_by(region, year, sex, age) %>% 
  summarise(dx_cnf = n()) %>% 
  ungroup() %>% 
  filter(region != "Israel",
         year >= 2012)

pse_cnf <- 
  bts %>% 
  filter(year <= 2023) %>% 
  reframe(dx_cnf = sum(dx_cnf),
          .by = c(year, sex, age)) %>% 
  mutate(region = "Palestine")

cnf2 <- 
  bind_rows(pse_cnf, bts) %>% 
  bind_rows(bind_rows(pse_cnf, bts) %>% 
              reframe(dx_cnf = sum(dx_cnf),
                      .by = c(year, region, age)) %>% 
              mutate(sex = "t"))

mx_out <- 
  mx %>% 
  left_join(cnf2) %>% 
  replace_na(list(dx_cnf = 0)) %>% 
  mutate(dx_noc = dx - dx_cnf,
         mx_noc = 1e5*dx_noc/pop)

mx_out %>% 
  filter(year %in% c(2012, 2014, 2018, 2022),
         sex == "t") %>% 
  ggplot()+
  geom_line(aes(age, mx_noc, col = region))+
  scale_y_log10()+
  facet_grid(source~year)+
  theme_bw()

write_rds(mx_out, "R/data_cleaning/data_inter/master_regional_deaths_population_age_sex_2012_2024.rds")
# write_rds(q0_gz2, "data_inter/un_igme_q01_q05_age_sex_2012_2022.rds")

### TEMPORARY FOR 2024-2025

mx_out <- read_rds("R/data_cleaning/data_inter/master_regional_deaths_population_age_sex_2012_2024.rds")

mx_out2 <- mx_out %>%
  rbind(mx_out %>%
          filter(year == 2024 & region == "Gaza Strip" & source == "pcbs") %>%
          mutate(year = 2025))

write_rds(mx_out2, file = "R/data_cleaning/data_inter/master_regional_deaths_population_age_sex_2012_2025_temp.rds")
