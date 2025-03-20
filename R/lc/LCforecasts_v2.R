
##--------------------------------------------------##
##
## LC forecasts on Palestine project
## 
##--------------------------------------------------##


## cleaning the workspace
rm(list=ls(all=TRUE))

## set up the directory where .R is saved (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## packages
library(tidyverse)
library(fields)
library(forecast)

## loading LC functions
source("funs/LCfuns.R")
source("funs/LifetableMX.R")

## loading data
data <- readRDS("master_regional_deaths_population_age_sex_2012_2024.rds")

## setting up the data 

## ages
x <- unique(data$age)
m <- length(x)

## regions, sources and sexes
regions <- unique(data$region)
sources <- unique(data$source)
sexes <- unique(data$sex)

## dataframe where to add results
data.with.fore <- data

## testing 
# tt <- r <- so <- se <- 1

## start big for loop

## repeat LC iterations for two fitting periods
for (tt in 1:2){
  ## fitting period
  if (tt == 1){
    t <- 2012:2019
    n <- length(t)
  }else{
    t <- 2012:2022
    n <- length(t)
  }
  ## forecast period
  t.fore <- (t[n]+1):2024
  n.fore <- length(t.fore)

  ## for loop to run over different sexes, regions and data sources
  for (r in 1:length(regions)){
    ## select region
    my.reg <- regions[r]
    for (so in 1:length(sources)){
      ## select source
      my.source <- sources[so]
      for (se in 1:length(sexes)){
        ## select sex
        my.sex <- sexes[se]
        
        ## LC routine ------
        
        ## extract deaths and exposures of interest
        y <- data %>% 
          filter(sex==my.sex,region==my.reg,source==my.source) %>% 
          filter(year%in%t) %>% 
          select(dx_noc) %>% pull()
        e <- data %>% 
          filter(sex==my.sex,region==my.reg,source==my.source) %>% 
          filter(year%in%t) %>%
          select(pop) %>% pull()
        Y <- matrix(y,m,n)
        E <- matrix(e,m,n)
        LMX <- log(Y/E)
        
        ## check evolution of e0
        e0 <- apply(exp(LMX), 2, e0.mx, x=x, sex="F")
        # plot(t,e0)
        
        ## plotting
        # matplot(x,LMX,t="l",lty=1,col=rainbow(n))
        # matplot(t,t(LMX),t="l",lty=1,col=rainbow(m))
        
        ## starting LC parameters   
        start.pars <- LC_starting_pars(Dth=Y,Exp=E)
        Alpha <- start.pars$Alpha
        Beta <- start.pars$Beta
        Kappa <- start.pars$Kappa
        One <- start.pars$One
        Eta <- Alpha %*% t(One) + Beta %*% t(Kappa)
        ## compute fitted deaths 
        D.fit <- E * exp(Eta)
        
        ## fit LC
        ## starting the iteration 
        for (iter in 1:1000){
          Alpha.old <- Alpha 
          Beta.old <- Beta 
          Kappa.old <- Kappa
          ## update Alpha
          temp <- Update.alpha(Alpha, Beta, Kappa,
                               One, Dth = Y, Exp = E, D.fit)
          D.fit <- temp$D.fit 
          Alpha <- temp$Alpha
          ## update Beta
          temp <- Update.beta(Alpha, Beta, Kappa,
                              One, Dth = Y, Exp = E, D.fit)
          D.fit <- temp$D.fit 
          Beta <- temp$Beta
          ## update Kappa
          temp <- Update.kappa(Alpha, Beta, Kappa,
                               One, Dth = Y, Exp = E, D.fit)
          
          D.fit <- temp$D.fit 
          Kappa <- temp$Kappa
          ## tolerance criterion
          crit <- max(max(abs(Alpha - Alpha.old)),
                      max(abs(Beta - Beta.old)), 
                      max(abs(Kappa - Kappa.old)))
          # cat(iter,crit,"\n")
          if(crit < 1e-04) break
          
        }
        
        ## adding the constraints 
        sum.Beta <- sum(Beta)
        Beta <- Beta / sum.Beta 
        Kappa <- Kappa * sum.Beta
        
        ## plotting LC parameters
        # par(mfrow=c(1,3))
        # plot(x,Alpha)
        # plot(x,Beta);abline(h=0)
        # plot(t,Kappa);abline(h=0)
        # par(mfrow=c(1,1))
        
        ## forecasting kappa index
        
        ## set a RW model with drift
        modK <- Arima(ts(Kappa,start = t[1]), order=c(0,1,0), include.drift=TRUE)
        ## forecast Kappa until 2040 
        predK <- forecast(modK,h=n.fore)
        ## plotting the time-series and forecast 
        # plot(predK)
        ## forecast Kappa
        KappaF <- predK$mean
        ## forecast Eta
        OneF <- matrix(1, nrow=n.fore, ncol = 1) 
        LMX.fore <- Alpha %*% t(OneF) + Beta %*% t(KappaF)
        
        ## plotting
        # cols <- viridis(n+n.fore)
        # matplot(x,LMX,t="l",lty=1,col=cols[1:n])
        # matlines(x,LMX.fore,lty=1,col=cols[1:n.fore+n])
        
        ## life expectancy
        e0.fore <- apply(exp(LMX.fore),2, e0.mx, x=x, sex="F")
        
        ## plotting e0
        # plot(t,e0,ylim=range(e0,e0.fore),xlim=range(t,t.fore))
        # points(t.fore,e0.fore,col=4,lwd=2,pch=16)
        
        ## plotting rates
        # whi.age <- 80
        # plot(t,LMX[which(x==whi.age),],xlim=range(t,t.fore),
        #      ylim=range(LMX[which(x==whi.age),],LMX.fore[which(x==whi.age),]))
        # points(t.fore,LMX.fore[which(x==whi.age),],col=4,lwd=2,pch=16)
        
        ## results in tibble
        df.fore.temp <- tibble(year=rep(t.fore,each=m),
                               sex=my.sex,
                               age=rep(x,n.fore),
                               region=my.reg,
                               dx=NA,
                               pop=NA,
                               mx=NA,
                               source=paste0("lc_",my.source,"_",t[n]),
                               dx_cnf=NA,
                               dx_noc=NA,
                               mx_noc=c(1e5*exp(LMX.fore)))
        
        ## adding results to data
        data.with.fore <- data.with.fore %>% 
          bind_rows(df.fore.temp)
        
      }
    }
  }

}
  
  
## plotting e0 outcomes
data.with.fore %>% 
  ## remove 2023 from wpp
  filter(!(source %in% c("wpp2024","pcbs") & year %in% c(2023,2024))) %>% 
  # filter(source=="lc") %>% 
  group_by(year,sex,region,source) %>% 
  summarise(e0=e0.mx(x=x,mx=mx_noc/1e5)) %>% 
  ggplot(aes(x=year,y=e0,color=source))+
  geom_vline(xintercept = 2019,linetype="dotted") +
  geom_vline(xintercept = 2022,linetype="dotted") +
  geom_point()+
  geom_line()+
  facet_wrap(region~sex,scales = "free_y") +
  scale_x_continuous(breaks = seq(t[1],max(t.fore),2))
ggsave("e0fore_v2.pdf",width = 12,height=8)  

## saving data
saveRDS(data.with.fore, file = "data_plus_forecasts_v2.rds")

## END



