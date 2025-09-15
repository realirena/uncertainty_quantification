rm(list=ls())
library(tidyverse)
library(readxl)

# Gaza 2017-2022
qs_gz <- read_xlsx("R/data_cleaning/data_input/input_data_gaza.xlsx",
                   sheet = "UN-IGME 2023 - Palestine")

qs_gz2 <- 
  qs_gz %>% 
  rename_with(tolower) %>% 
  mutate(sex = str_sub(sex, 1, 1) %>% str_to_lower()) %>% 
  filter(sex != "b")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# converting to mx ====
# ~~~~~~~~~~~~~~~~~~~~~


# ages 1-4
# ~~~~~~~~
# Adapting Preston reference derived/adapted from Coale and Demeney
# obtaining ax from qx instead of mx

# males
tst_m <- 
  tibble(mx = seq(0, 0.107, 0.000001)) %>% 
  group_by() %>% 
  mutate(t = 1:n(),
         ax = 1.651 - 2.816 * mx,
         qx = 4*mx/(1+(4-ax)*mx))

lm(ax ~ qx, data = tst_m)$coefficients

# females
tst_f <- 
  tibble(mx = seq(0, 0.107, 0.0001)) %>% 
  group_by() %>% 
  mutate(t = 1:n(),
         ax = 1.522 - 1.518 * mx,
         qx = 4*mx/(1+(4-ax)*mx))

lm(ax ~ qx, data = tst_f)$coefficients

ax_1_4_a_m <- lm(ax ~ qx, data = tst_m)$coefficients[1]
ax_1_4_b_m <- lm(ax ~ qx, data = tst_m)$coefficients[2]

ax_1_4_a_f <- lm(ax ~ qx, data = tst_f)$coefficients[1]
ax_1_4_b_f <- lm(ax ~ qx, data = tst_f)$coefficients[2]

qs_gz3 <- 
  qs_gz2 %>% 
  mutate(
    q1_4 = 1-(1-q0_5)/(1-q0_1),
    a0_1 = case_when(sex == "f" & q0_1 < 0.0170 ~ 0.1490 - 2.0867 * q0_1,
                          sex == "f" & q0_1 >= 0.0170 & q0_1 < 0.0658 ~ 0.0438 + 4.1075 * q0_1,
                          sex == "f" & q0_1 >= 0.0658 ~ 0.3141,
                          sex == "m" & q0_1 < 0.0226 ~ 0.1493 - 2.0367 * q0_1,
                          sex == "m" & q0_1 >= 0.0226 & q0_1 < 0.0785 ~ 0.0244 + 3.4994 * q0_1,
                     sex == "m" & q0_1 >= 0.0785 ~ 0.2991),
    a1_4 = case_when(sex == "f" & q0_1 < 0.1 ~ ax_1_4_a_f + ax_1_4_b_f * q0_1,
                     sex == "f" & q0_1 >= 0.1 & q0_1 < 0.0785 ~ 0.0244 + 3.4994 * q0_1,
                     sex == "m" & q0_1 < 0.1 ~ ax_1_4_a_m + ax_1_4_b_m * q0_1,
                     sex == "m" & q0_1 >= 0.1 & q0_1 < 0.0785 ~ 0.0244 + 3.4994 * q0_1),
    m0_1 = q0_1/(1-q0_1*(1-a0_1)),
    m1_4 = q1_4/(4-q1_4*(4-a1_4)))

qs_gz4 <- 
  qs_gz3 %>% 
  filter(year %in% 2012:2024) %>% 
  select(year, sex, m0_1, m1_4) %>% 
  gather(m0_1, m1_4, key = age, value = mx) %>% 
  mutate(age = ifelse(age == "m0_1", 0, 1))
  
write_rds(qs_gz4, "R/data_cleaning/data_inter/infant_child_mortality_rates_igme.rds")


