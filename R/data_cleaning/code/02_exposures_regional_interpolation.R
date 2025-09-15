rm(list=ls())
library(tidyverse)
library(readxl)
library(data.table)
library(mgcv)


# Gaza 2017-2022
pop_gz <- read_xlsx("R/data_cleaning/data_input/input_data_gaza.xlsx",
                   sheet = "Population")

unique(pop_gz$AgeStart)

pop_gz2 <- 
  pop_gz %>% 
  rename_with(tolower) %>% 
  select(year, sex, age = agestart, pop = population) %>% 
  mutate(sex = str_sub(sex, 1, 1) %>% str_to_lower(),
         sex = ifelse(sex == "b", "t", sex),
         age = ifelse(age >= 75, 75, age)) %>% 
  reframe(pop = sum(pop),
          .by = c(year, sex, age)) %>% 
  mutate(region = "Gaza Strip",
         source = "gaza")


# PSCB
dt <- 
  read_rds("R/data_cleaning/data_inter/pcbs_exposures_region.rds") %>% 
  rename_with(tolower) %>% 
  ungroup() %>% 
  rename(age = age5,
         pop = population) %>% 
  mutate(sex = str_sub(sex, 1, 1) %>% str_to_lower(), 
         source = "Census") 
  
unique(dt$region)
unique(dt$sex)
unique(dt$year)
unique(dt$age)

tot <- 
  dt %>%
  reframe(pop = sum(pop),
          .by = c(region, year, sex)) %>% 
  filter(year >= 2013)

dt %>%
  reframe(pop = sum(pop),
          .by = c(region, year, sex)) %>% 
  ggplot()+
  facet_wrap(region~sex, scales = "free_y")+
  geom_line(aes(year, pop, col = region))

dt %>%
  filter(age %in% seq(10, 80, 10),
         region == "Palestine") %>% 
  ggplot()+
  facet_wrap(age~sex, scales = "free_y")+
  geom_point(aes(year, pop, col = region))

dt %>%
  reframe(pop = sum(pop),
          .by = c(region, year, sex)) %>% 
  ggplot()+
  facet_wrap(region~sex, scales = "free_y")+
  geom_point(aes(year, pop, col = region))



# world population prospects #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wpp <- 
  fread(file.path("R/data_cleaning/data_input", 
                  "WPP2024_PopulationBySingleAgeSex_Medium_1950-2023.csv.gz"))

wpp2 <- 
  wpp %>% 
  rename_with(tolower) %>% 
  select(iso = iso3_code, 
         year = time, age = agegrp, m = popmale, f = popfemale, t = poptotal) %>% 
  filter(iso == "PSE") %>% 
  as_tibble() %>% 
  filter(year %in% 1997:2024) %>% 
  gather(m, f, t, key = sex, value = pop) %>% 
  mutate(pop = as.double(pop) * 1000, 
         age = ifelse(age == "100+", 100, as.double(age)))

unique(dt$age)

wpp3 <- 
  wpp2 %>% 
  mutate(age = case_when(age %in% 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age >= 75 ~ 75,
                         TRUE ~ age - age%%5)) %>% 
  reframe(pop = sum(pop),
          .by = c(year, sex, age)) %>% 
  mutate(source = "wpp",
         region = "Palestine")

unique(dt$age)
unique(wpp3$age)


# US Census bureau
# ~~~~~~~~~~~~~~~~
# data from the US census bureau in 5-year age groups
dt_cb_s <- read_csv("R/data_cleaning/data_input/pse_pop_sex_single_age_territory_2000_2024_uscensus.csv")

dt_cb2 <- 
  dt_cb_s %>% 
  select(region = 1,
         year = 2,
         age = 3,
         t = 5,
         m = 7,
         f = 9) %>% 
  gather(t, m, f, key = sex, value = pop) %>% 
  drop_na(age) %>% 
  mutate(pop = str_replace(pop, "\\.", ""),
         pop = pop %>% as.double(),
         age = ifelse(age == "100+", 100, as.integer(age)))

# adding total for Palestine
dt_cb3 <- 
  dt_cb2 %>% 
  bind_rows(dt_cb2 %>% 
              reframe(pop = sum(pop),
                      .by = c(year, age, sex)) %>% 
              mutate(region = "Palestine"))

# grouping ages to compare
dt_cb4 <- 
  dt_cb3 %>% 
  mutate(age = case_when(age == 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age >= 75 ~ 75,
                         TRUE ~ age - age%%5)) %>% 
  reframe(pop = sum(pop),
          .by = c(region, year, age, sex)) %>% 
  mutate(source = "uscb")


unique(dt_cb4$region)
unique(dt_cb3$sex)


# putting all together ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~
dt_comp <- 
  dt %>% 
  bind_rows(dt_cb4) %>% 
  bind_rows(wpp3) %>% 
  bind_rows(pop_gz2)


dt_comp %>% 
  group_by(region, source, year, sex) %>% 
  summarise(pop = sum(pop)) %>% 
  ggplot()+
  geom_point(aes(year, pop, col = source))+
  geom_line(aes(year, pop, col = source))+
  facet_wrap(region~sex, scales = "free_y")+
  theme_bw()

dt_comp %>% 
  filter(region != "West Bank",
         age %in% seq(10, 70, 10),
         sex == "t") %>% 
  ggplot()+
  geom_point(aes(year, pop, col = source))+
  geom_line(aes(year, pop, col = source))+
  facet_wrap(region~age, scales = "free_y")+
  theme_bw()

dt_comp %>% 
  filter(age %in% seq(10, 70, 20),
         sex != "t") %>% 
  ggplot()+
  geom_point(aes(year, pop, col = source))+
  geom_line(aes(year, pop, col = source))+
  facet_grid(sex+age~region, scales = "free_y")+
  theme_bw()

dt_comp %>% 
  filter(region != "West Bank",
         year == 2005,
         sex != "t") %>%
  mutate(pop = ifelse(sex == "m", -pop, pop)) %>% 
  ggplot()+
  # geom_point(aes(year, pop, col = source, group = interaction(source, sex)))+
  geom_line(aes(age, pop, col = source, group = interaction(source, sex)))+
  geom_hline(yintercept = 0, lty = "dashed")+
  facet_wrap(~region, scales = "free_x")+
  coord_flip()+
  theme_bw()

# population interpolation ====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(dt$age)
unique(dt$sex)

rg <- "West Bank"
sx <- "f"
ag <- 70

inter_pop <- function(yrs = c(1987:2001)){
  
  rt_ctf <- tibble()
  
  ags <- unique(dt$age)
  sxs <- unique(dt$sex)
  rgs <- unique(dt$region)
  
  for(rg in rgs){
    
    tmp1 <- 
      dt %>% 
      filter(region == rg)
    
    for(sx in sxs){
      
      tmp2 <- 
        tmp1 %>% 
        filter(sex == sx)
      
      for(ag in ags){
        
        tmp3 <- 
          tmp2 %>% 
          filter(age == ag) %>% 
          complete(region, year = min(yrs):max(yrs), sex, age, source, fill = list(pop = NA)) %>% 
          mutate(pop2 = ifelse(year %in% yrs, pop, NA),
                 w = ifelse(year %in% yrs, 1, 0))
        
        md <- gam(pop2 ~ s(year, bs = 'ps'), 
                  weights = w,
                  data = tmp3,
                  family = "quasipoisson")
        
        tmp4 <- 
          tmp3 %>% 
          mutate(pop_int = predict(md, newdata = tmp3, type = "response"))
        
        rt_ctf <- 
          rt_ctf %>% 
          bind_rows(tmp4)
        
      }
    }
  }
  
  dt_out <- 
    dt %>% 
    left_join(rt_ctf)
  
  return(dt_out)
}

pop_int <- inter_pop(yrs = c(1997:2000, 2009:2010, 2017:2023))

pop_int %>% 
  gather(pop, pop_int, key = scenario, value = pop) %>% 
  filter(age %in% seq(10, 70, 10),
         # year >= 2010,
         sex == "t") %>% 
  ggplot()+
  # geom_point(aes(year, pop, col = scenario))+
  geom_line(aes(year, pop, col = scenario))+
  geom_vline(xintercept = 2017, lty = "dashed")+
  facet_wrap(region~age, scales = "free_y")+
  theme_bw()

pop_int %>% 
  gather(pop, pop_int, key = scenario, value = pop) %>% 
  filter(age %in% seq(10, 70, 10),
         # year >= 2005,
         sex == "f") %>% 
  ggplot()+
  # geom_point(aes(year, pop, col = scenario))+
  geom_vline(xintercept = 2017, lty = "dashed")+
  geom_line(aes(year, pop, col = scenario))+
  facet_wrap(region~age, scales = "free_y")+
  theme_bw()

dt_comp %>% 
  filter(age %in% seq(10, 70, 10),
         year >= 2010,
         sex == "t") %>% 
  ggplot()+
  geom_point(aes(year, pop, col = source))+
  geom_line(aes(year, pop, col = source))+
  facet_wrap(region~age, scales = "free_y")+
  theme_bw()

pop_int %>% 
  gather(pop, pop_int, key = scenario, value = pop) %>% 
  group_by(year, sex, region, scenario) %>% 
  summarise(pop = sum(pop)) %>% 
  filter(year >= 2010) %>% 
  ggplot()+
  # geom_point(aes(year, pop, col = scenario))+
  geom_line(aes(year, pop, col = scenario))+
  facet_wrap(region~sex, scales = "free_y")+
  theme_bw()

unique(pop_int$age)

pop_int2 <- 
  pop_int %>% 
  select(-w, -pop2, -source) %>% 
  # keeping the population as observed in 2024
  mutate(pop_int = ifelse(year == 2024, pop, pop_int))

pop_int2 %>% 
  gather(pop, pop_int, key = scenario, value = pop) %>% 
  group_by(year, sex, region, scenario) %>% 
  summarise(pop = sum(pop)) %>% 
  filter(year >= 2010) %>% 
  ggplot()+
  # geom_point(aes(year, pop, col = scenario))+
  geom_line(aes(year, pop, col = scenario))+
  facet_wrap(region~sex, scales = "free_y")+
  theme_bw()

# opening age group until 80+
# function for ungrouping ages into single groups
# using the function PCLM from the ungroup package
library(ungroup)
split_ages <- function(db, lambda = 100){
  
  age <- db$age
  pop <- db$pop_int
  
  nlast <- 101 - max(age)
  
  # without offset
  V1 <- pclm(x = age, 
             y = pop, 
             nlast = nlast)$fitted
  
  tibble(age = seq(0, 100, 1), pop = V1) 
  
}

pop_spl <- 
  pop_int2 %>% 
  group_by(region, year, sex) %>% 
  do(split_ages(db = .data)) %>% 
  ungroup()

pop_spl_gr <- 
  pop_spl %>% 
  mutate(age = case_when(age %in% 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age %in% 5:74 ~ age-age%%5,
                         age>= 75 ~ 75)) %>% 
  reframe(pop_gr = sum(pop),
          .by = c(region, year, sex, age))

pop_int3 <- 
  pop_int2 %>% 
  left_join(pop_spl_gr)

pop_out <- 
  pop_spl %>% 
  mutate(age = case_when(age %in% 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age %in% 5:79 ~ age-age%%5,
                         age>= 80 ~ 80)) %>% 
  reframe(pop = sum(pop),
          .by = c(region, year, sex, age))

write_rds(pop_out, "R/data_cleaning/data_inter/pcbs_exposures_region_interpol_open_age80.rds")

pop_gz3 <- 
  pop_gz %>% 
  rename_with(tolower) %>% 
  select(year, sex, age = agestart, pop = population) %>% 
  mutate(sex = str_sub(sex, 1, 1) %>% str_to_lower(),
         sex = ifelse(sex == "b", "t", sex)) %>% 
  mutate(region = "Gaza Strip",
         source = "pcbs_gaza")

write_rds(pop_gz3, "R/data_cleaning/data_inter/pcbs_exposures_gaza_guillot.rds")


gza_cmp <- 
  pop_out %>% 
  filter(region == "Gaza Strip") %>% 
  mutate(source = "inter") %>% 
  bind_rows(pop_gz3)

gza_cmp %>% 
  group_by(year, sex, source) %>% 
  summarise(pop = sum(pop)) %>% 
  ggplot()+
  geom_line(aes(year, pop, col = source))+
  facet_wrap(~sex)
