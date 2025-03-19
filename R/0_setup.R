library(tidyverse)
library(lubridate)
library(readxl)
library(ungroup)
library(mgcv)
library(data.table)
options(scipen=99999)

# dts <- dt
# pop <- pop2
# yrs <- c(1989, 2023)

# function to split CDR into mx for mortality crises
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
crd_to_mx2 <- function(dts, cts, pop, as, evs, yrs){
  
  as2 <- 
    as %>% 
    filter(eventype %in% evs)
  
  # assuming civilians as deaths in conflict
  conf <- 
    dts %>% 
    filter(country %in% cts,
           year %in% yrs) %>% 
    mutate(eventype = case_when(role == "civilians" ~ "Conflict",
                                role == "combatants" ~ "Conflict: combatant")) %>% 
    left_join(as2, 
              by = "eventype",
              relationship = "many-to-many") %>% 
    select(-country) %>% 
    left_join(pop %>% filter(year %in% yrs),
              by = join_by(year, code, sex, age)) %>% 
    mutate(dx_sd = rr * pop / 1000,
           dx_sd_ll = rr_ll * pop / 1000,
           dx_sd_ul = rr_ul * pop / 1000,
           scenario = "only conflict")
  
  # assuming civilians as deaths in genocide
  genc <- 
    dts %>% 
    filter(country %in% cts,
           year %in% yrs) %>% 
    mutate(eventype = case_when(role == "civilians" ~ "Genocide",
                                role == "combatants" ~ "Conflict: combatant")) %>% 
    left_join(as2, 
              by = "eventype",
              relationship = "many-to-many") %>% 
    select(-country) %>% 
    left_join(pop %>% filter(year %in% yrs),
              by = join_by(year, code, sex, age)) %>% 
    mutate(dx_sd = rr * pop / 1000,
           dx_sd_ll = rr_ll * pop / 1000,
           dx_sd_ul = rr_ul * pop / 1000,
           scenario = "conflict and genocide")
  
  # adjustment of death rates for total sex
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dts3 <- 
    bind_rows(conf, genc) %>% 
    filter(!is.na(country)) %>%
    mutate(inc_t = ifelse(sex == "t", 1, 0),
           inc_mf = ifelse(sex == "t", 0, 1)) %>% 
    group_by(year, country, scenario, eventype) %>% 
    mutate(dx_sd_t_sum = sum(inc_t * dx_sd),
           dx_sd_mf_sum = sum(inc_mf * dx_sd)) %>% 
    ungroup() %>% 
    mutate(dx = ifelse(sex == "t",
                       dx_sd*dts/dx_sd_t_sum,
                       dx_sd*dts/dx_sd_mf_sum),
           dx_ll = ifelse(sex == "t",
                          dx_sd_ll*dts/dx_sd_t_sum,
                          dx_sd_ll*dts/dx_sd_mf_sum),
           dx_ul = ifelse(sex == "t",
                          dx_sd_ul*dts/dx_sd_t_sum,
                          dx_sd_ul*dts/dx_sd_mf_sum)) %>% 
    select(country, code, year, everything(), 
           -starts_with("rr"), 
           -starts_with("dx_sd"), 
           -starts_with("inc"))
  
  dts4 <- 
    dts3 %>% 
    bind_rows(dts3 %>% 
                reframe(dts = sum(dts),
                        pop = mean(pop),
                        dx = sum(dx),
                        dx_ll = sum(dx_ll),
                        dx_ul = sum(dx_ul),
                        .by = c(country, code, scenario, year, sex, age)) %>% 
                mutate(role = "all", eventype = "all")) %>% 
    mutate(mx = 1e5*dx/pop,
           mx_ll = 1e5*dx_ll/pop,
           mx_ul = 1e5*dx_ul/pop) %>% 
    select(country, code, scenario, role, year, sex, age, everything()) %>% 
    arrange(country, code, scenario, role, year, sex, age)
    
  return(dts4)
}

ung_age <- function(chunk){
  dt_in <- 
    tibble(age = chunk$age,
           dx = chunk$dx,
           dx_mt = dx*1e5) %>% 
    mutate(dx_mt = ifelse(dx_mt == 0, 1, dx_mt))
  nl <- 26
  dxs <- pclm(x = dt_in$age,
              y = dt_in$dx_mt, 
              nlast = nl)$fitted
  fit <- tibble(age = 0:100, dx = dxs/1e5)
  out <- 
    chunk %>% 
    select(code, country, year, scenario, eventype,
           role, sex) %>% 
    unique() %>%
    left_join(fit,
              by = character())
  return(out)
}

ung_age2 <- function(chunk){
  dt_in <- 
    tibble(age = chunk$age,
           dx = chunk$dx,
           dx_mt = dx*1e5) %>% 
    mutate(dx_mt = ifelse(dx_mt == 0, 1, dx_mt))
  nl <- 26
  dxs <- pclm(x = dt_in$age,
              y = dt_in$dx_mt, 
              nlast = nl)$fitted
  fit <- tibble(age = 0:100, dx = dxs/1e5)
  out <- 
    chunk %>% 
    select(code, country, year, sex) %>% 
    unique() %>%
    left_join(fit,
              by = character())
  return(out)
}

# ------Life table from mortality rates------#
# Authors: Carlo-Giovanni Camarda and Ugofilippo Basellini
lifetable.mx <- function(x, mx, sex="m", ax=NULL){
  m <- length(x)
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="f"){
        if(mx[1] < 0.01724){
          ax[1] <- 0.14903 - 2.05527 * mx[1]
        }else if (mx[1] >= 0.01724 & mx[1] < 0.06891){
          ax[1] <- 0.04667 + 3.88089 * mx[1]
        } else {
          ax[1] <- 0.31411
        }
      }
      if(sex=="m"){
        if(mx[1] < 0.02300){
          ax[1] <- 0.14929 - 1.99545 * mx[1]
        }else if(mx[1] >= 0.02300 & mx[1] < 0.08307){
          ax[1] <- 0.02832 + 3.26021 * mx[1]
        } else{
          ax[1] <- 0.29915
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  qx  <- n*mx / (1 + (n - ax) * mx)
  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

## grab the life expectancy (by sex) at age 0 
get_le0_dt <- function(lifetable, sex=NULL, year, scenario, le0){ 
  ## also use males for total LE computations (for now)
  lifetable_age0 <- lifetable[lifetable$x==0,]
  lifetable_age0$year <- year
  lifetable_age0$sex <- sex
  lifetable_age0$scenario <- scenario
  if(sex=="Females"){
    lifetable_age0$bmmr_lss <-	le0[1] -  lifetable_age0$ex   ## LSS for women
  } else if(sex=="Males"){
    lifetable_age0$bmmr_lss <-	le0[2] - lifetable_age0$ex     ## LSS for men 
  } else{
    lifetable_age0$bmmr_lss <- 	le0[3] - lifetable_age0$ex  ## total LSS
  }
  return(lifetable_age0)
}


