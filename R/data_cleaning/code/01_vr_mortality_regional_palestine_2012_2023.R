rm(list=ls())
# library(ungroup)
library(tidyverse)
library(readxl)

# loading mortality data from VR

# Palestine 2012-2022 (from UNPD)
dt_unpd <- read_rds("R/data_cleaning/data_inter/deaths_palestine_unpd_2012_2022.rds")

# West Bank 2012-2017, 2022
dt_pse <- read_xlsx("R/data_cleaning/data_input/deaths_palestine_age_sex_2012_2023_pcbs.xlsx")

# Gaza 2017-2022
dt_gz <- read_xlsx("R/data_cleaning/data_input/input_data_gaza.xlsx",
                   sheet = "VR-Deaths")


# UNPD
unique(dt_unpd$age)
dt_unpd %>% 
  filter(sex != "t") %>% 
  reframe(dts = sum(deaths),
          .by = c(year)) %>% 
  arrange(year)

dt_unpd2 <- 
  dt_unpd %>% 
  filter(age >= 0) %>% 
  mutate(region = "Palestine",
         sex = str_sub(sex, 1, 1),
         source = "unpd") %>% 
  select(region, year, sex, age, dx = deaths, source) %>% 
  arrange(region, year, sex, age)

  
# Palestine 2012 - 2022
dt_pse2 <- 
  dt_pse %>% 
  filter(region == "West Bank") %>% 
  separate(age, c("age", "trash"), sep = "-") %>% 
  select(-trash) %>% 
  gather(females, males, total, key = sex, value = dx) %>% 
  mutate(age = ifelse(age == 1, 0, age)) %>% 
  reframe(dx = sum(dx),
          .by = c(year, region, sex, age))

# testing totals are equivalent to age-specifics
dt_pse2 %>% 
  filter(age != "Total") %>% 
  left_join(dt_pse2 %>% 
              filter(age == "Total") %>% 
              select(-age) %>% 
              rename(dx_tot = dx)) %>% 
  group_by(year, sex, region) %>% 
  mutate(dx_sum = sum(dx),
         diff = dx_tot - dx_sum) %>% 
  filter(diff != 0)

dt_pse2 %>% 
  filter(sex != "total") %>% 
  left_join(dt_pse2 %>% 
              filter(sex == "total") %>% 
              select(-sex) %>% 
              rename(dx_tot = dx)) %>% 
  group_by(year, age, region) %>% 
  mutate(dx_sum = sum(dx),
         diff = dx_tot - dx_sum) %>% 
  filter(diff != 0)

dt_pse3 <- 
  dt_pse2 %>% 
  filter(age != "Total") %>% 
  mutate(sex = str_sub(sex, 1, 1), 
         age = ifelse(age == "80+", 80, age %>% as.double),
         source = "pcbs")

unique(dt_pse3$age)
unique(dt_pse3$sex)
unique(dt_pse3$region)


# Gaza 2017-2022
dt_gz2 <- 
  dt_gz %>% 
  select(year = Year,
         age = AgeStart,
         sex = Sex,
         dx = Deaths) %>% 
  mutate(sex = str_to_lower(sex),
         sex = str_sub(sex, 1, 1),
         sex = ifelse(sex == "b", "t", sex),
         age = ifelse(age == 1, 0, age)) %>% 
  reframe(dx = sum(dx),
          .by = c(year, sex, age))

# adjustinf sexes in Guillot file
dt_gz2 %>% 
  filter(sex != "t") %>% 
  group_by(year, sex) %>% 
  summarise(dts = sum(dx))

dt_gz3 <- 
  dt_gz2 %>% 
  mutate(sex2 = case_when(year %in% 2018:2020 & sex == "m" ~ "f",
                          year %in% 2018:2020 & sex == "f" ~ "m",
                          TRUE ~ sex)) %>% 
  select(year, sex = sex2, age, dx)

dt <- 
  dt_unpd2 %>% 
  mutate(age = ifelse(age < 80, age - age%%5, 80)) %>% 
  reframe(dx_pse = sum(dx),
          .by = c(year, sex, age)) %>% 
  left_join(dt_pse3 %>%  
              select(year, sex, age, dx_wbk = dx)) %>% 
  left_join(dt_gz3 %>%  
              select(year, sex, age, dx_gza = dx)) %>% 
  mutate(dx_gza = ifelse(is.na(dx_gza), dx_pse - dx_wbk, dx_gza),
         dx_gza = ifelse(dx_gza >= 0, dx_gza, 0),
         dx_wbk = ifelse(is.na(dx_wbk), dx_pse - dx_gza, dx_wbk),
         dx_wbk = ifelse(dx_wbk >= 0, dx_wbk, 0),
         dx_pse2 = (dx_gza + dx_wbk),
         dx_gza2 = dx_pse - dx_wbk,
         diff_pse = dx_pse - dx_pse2,
         diff_gza = dx_gza - dx_gza2)

dt_out <- 
  dt %>% 
  select(year, sex, age, dx_pse, dx_gza, dx_wbk) %>% 
  gather(starts_with("dx"), key = region, value = dx) %>% 
  mutate(region = case_when(region == "dx_pse" ~ "Palestine",
                            region == "dx_gza" ~ "Gaza Strip",
                            region == "dx_wbk" ~ "West Bank")) %>% 
  # adding deaths in West Bank in 2023
  bind_rows(dt_pse3 %>% 
              filter(year == 2023) %>% 
              select(-source))



write_rds(dt_out, "R/data_cleaning/data_inter/vr_deaths_regions_palestine_2012_2023.rds")

