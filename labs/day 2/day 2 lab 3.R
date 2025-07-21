# Lab Session 6: Distributed Lag Non-linear Models
# Marianthi-Anna Kioumourtzoglou
# Sebastian Rowland
# Advanced Analytic Methods for Environmental Health Sciences

####***********************
#### Table of Contents ####
####***********************

# R: Define Research Question 
# 0: Preparation 
# 2: Prepare NY Daily Data
# 3: Individual Models of Each Lag
# 4: Unconstrained Distributed Lags
# 5: Linear Constrained DLM 
# 6: Nonlinear Constrained DLNM
# Footnote: Too Many df for Lag Dimension
# Footnote: Examining Different df for lag dimension

####*********************************
#### R: Define Research Question ####
####*********************************

# Can daily exposure to PM2.5 increase the rate of hospitalization for 
# cardiovascular events? 
# If we decreased daily PM2.5 for a day, would cardiovascular hospitalizations 
# decrease? By how much? 

####********************
#### 0: Preparation ####
####********************

# 0a Install packages 

# the dlnm package provides a set of functions to 
# create and plot 
# distributed lag nonlinear models 

#install.packages("dlnm")

# 0b Load packages

library(readr)
library(dplyr) 
library(lubridate)
library(splines)
library(ggplot2)
library(dlnm)

library(here)

#0c Declare folder paths

ProjectFolder <- here("data")

####*******************************
#### 1: Prepare NY Daily Data #####
####*******************************

# 1a Readin data 

dta <- read_csv(here::here(ProjectFolder, "nyc_time_series.csv"))

# 1b Look at data structure 

####*******************************************
# Variable Codebook https://www.health.ny.gov/statistics/sparcs/
# each row represents one day 
# dayD: the calendar date 
##   from the Air Quality System 
##   of the US Environmental Protection Agency 
##   https://aqs.epa.gov/aqsweb/airdata/download_files.html
# dailyPM: 24-hour mean PM2.5 concentration, population-weighted average of
#          monitor measurements 
#          units: ug/m^3
##   From the North American Land Data Assimilation System (NLDAS)
##   https://ldas.gsfc.nasa.gov/nldas/v2/forcing
# dailyTemp: population-weighted average of temperature 
#            units: Degrees Celcius 
# dailyRH: population-weighted average of relative humidity in percentage  
#          units: percentage
##   From the Statewide Planning And Research Cooperative System (SPARCS)
##   https://www.health.ny.gov/statistics/sparcs/
# dailyCVD: Number of cardiovascular-related hospitalizations that day 
#           units: count
####*******************************************

# 1c Convert date column to datetime format 

dta <- dta %>% 
  mutate(Date = parse_date_time(dateD, "mdy")) %>% 
             dplyr::select(-dateD)

# 1d Extract the day of the week

dta <- dta %>% mutate(DayofWeek = as.character(wday(Date, label =TRUE)))
head(dta$DayofWeek)

# 1e Arrange data by date 

dta <- dta %>% arrange(Date)

# 1f Compute the number of years 

yr_num <- length(unique(year(dta$Date)))

# 1g Plot Autocorrelation
# acf() computes and plots the autocorrelation 
# for each lag

acf(dta$dailyPM, lag.max = 7)

# there appears to be bit of a weekly pattern to the autocorrelation 
# same days of the week have more similar PM2.5

acf(dta$dailyPM, lag.max = 21)

####**************************************
#### 2: Individual Models of Each Lag ####
####**************************************

# 2a Construct lagged PM, temperature, RH
# we will use the lag() function to assign the lagged exposures 
# but this is not the only possible way to assign lags

# pm_Lag0 is the same variable as DailyPM 
# it is not necesary to create a new variable in order for the Lag 0 model,
# but it helps illustrate the lags. 

dta <- dta %>% mutate(
  pm_Lag0 = dailyPM,
  pm_Lag1 = lag(dailyPM, 1), 
  pm_Lag2 = lag(dailyPM, 2), 
  pm_Lag3 = lag(dailyPM, 3), 
  pm_Lag4 = lag(dailyPM, 4), 
  pm_Lag5 = lag(dailyPM, 5), 
  pm_Lag6 = lag(dailyPM, 6))

# repeat for temp and RH 

dta <- dta %>% mutate(
  temp_Lag0 = dailyTemp,
  temp_Lag1 = lag(dailyTemp, 1), 
  temp_Lag2 = lag(dailyTemp, 2), 
  temp_Lag3 = lag(dailyTemp, 3), 
  temp_Lag4 = lag(dailyTemp, 4), 
  temp_Lag5 = lag(dailyTemp, 5), 
  temp_Lag6 = lag(dailyTemp, 6), 
  rh_Lag0 = dailyRH,
  rh_Lag1 = lag(dailyRH, 1), 
  rh_Lag2 = lag(dailyRH, 2), 
  rh_Lag3 = lag(dailyRH, 3), 
  rh_Lag4 = lag(dailyRH, 4), 
  rh_Lag5 = lag(dailyRH, 5), 
  rh_Lag6 = lag(dailyRH, 6))

# 2b Look at data structure 

View(dta)

####****************************
##** Class Question 1 **##
# Do we have any missing data? Why is it missing? 
# How could we address this? 
##*****************************************************************## 


# 2c Construct the time series models 
# one for each lag 
# we will assume a linear dose-response relationship 
# between PM and CVD admission rate 
# and a nonlinear relationship for temperature and relative humidity 
# we will include indicator variables for day of the week
# again, we will use a spline term for secular trends 
# with 4 degrees of freedom per year

mod.lag0 <- glm(dailyCVD ~               # outcome
                  pm_Lag0 +                   # exposure
                  ns(temp_Lag0, df = 4) +     # nonlinear term for Temp
                  ns(rh_Lag0, df = 3) +       # nonlinear term for RH
                  DayofWeek +              
                  ns(Date, df = 4*yr_num), 
                family = "quasipoisson",   # distribution family
                data = dta)

##** Class Question 2 **##
# Why do we adjust for Day of the Week? 
# Why do we use a Quasi-Possion distribution? 
# What does ns() refer to?
##*****************************************************************## 


mod.lag1 <- glm(dailyCVD ~ pm_Lag1 +  ns(temp_Lag1, df = 4) + 
                  ns(rh_Lag1, df = 3) + DayofWeek + ns(Date, df = 4*yr_num), 
                family = "quasipoisson", data = dta)

##** Class Question 3 **##
# What is the difference between this model and the previous model?
##*****************************************************************## 


mod.lag2 <- glm(dailyCVD ~ pm_Lag2 +  ns(temp_Lag2, df = 4) + 
                  ns(rh_Lag2, df = 3) + DayofWeek + ns(Date, df = 4*yr_num), 
                family = "quasipoisson", data = dta)

mod.lag3 <- glm(dailyCVD ~ pm_Lag3 +  ns(temp_Lag3, df = 4) + 
                  ns(rh_Lag3, df = 3) + DayofWeek + ns(Date, df = 4*yr_num), 
                family = "quasipoisson", data = dta)

mod.lag4 <- glm(dailyCVD ~ pm_Lag4 +  ns(temp_Lag4, df = 4) + 
                  ns(rh_Lag4, df = 3) + DayofWeek + ns(Date, df = 4*yr_num), 
                family = "quasipoisson", data = dta)

mod.lag5 <- glm(dailyCVD ~ pm_Lag5 +  ns(temp_Lag5, df = 4) + 
                  ns(rh_Lag5, df = 3) + DayofWeek + ns(Date, df = 4*yr_num), 
                family = "quasipoisson", data = dta)

mod.lag6 <- glm(dailyCVD ~ pm_Lag6 +  ns(temp_Lag6, df = 4) + 
                  ns(rh_Lag6, df = 3) + DayofWeek + ns(Date, df = 4*yr_num), 
                family = "quasipoisson", data = dta)

# 2d Plot the models' results 

# 2d.i Extract the coefficients from each model 
# In this code I have condensed several steps: 
# we are summarizing each model, then extracting
# the coefficient and se of the second row (pm_0)
# of the coefficients matrix

coeff.lag0 <- summary(mod.lag0)$coefficients[2, 1:2]
coeff.lag1 <- summary(mod.lag1)$coefficients[2, 1:2]
coeff.lag2 <- summary(mod.lag2)$coefficients[2, 1:2]
coeff.lag3 <- summary(mod.lag3)$coefficients[2, 1:2]
coeff.lag4 <- summary(mod.lag4)$coefficients[2, 1:2]
coeff.lag5 <- summary(mod.lag5)$coefficients[2, 1:2]
coeff.lag6 <- summary(mod.lag6)$coefficients[2, 1:2]

# 2d.ii Create dataframe of model results

coeff.table <- rbind(coeff.lag0, coeff.lag1, coeff.lag2, coeff.lag3,
                     coeff.lag4, coeff.lag5, coeff.lag6)

coeff.table <- as.data.frame(coeff.table, stringsAsFactors = FALSE)

# 2d.iii Set names for dataframe

names(coeff.table) <- c("coeff", "se")

# 2d.iv Compute confidence intervals 

coeff.table <- coeff.table %>% 
  mutate(lci = coeff - 1.96 * se, 
         uci = coeff + 1.96 * se)

# 2d.v Exponeniate terms 
# since the link function is log() 
# when we exponantiate, we translate our coefficients 
# from log(RateRatio)'s
# to Rate Ratios 
# we will also multiply by 10 to compute the 
# the rate ratio per 10 ug/m3 increase in daily PM

coeff.table <- coeff.table %>% 
  mutate(coeff.rr = exp(10*coeff), 
         lci.rr = exp(10*lci), 
         uci.rr = exp(10*uci))

# 2d.vi Create model names 

coeff.table        <- coeff.table %>% 
  mutate(ModelName = c("Lag 0", "Lag 1", "Lag 2", "Lag 3", "Lag 4", "Lag 5", "Lag 6"))

##** Class Question 4 **##
# What is the interpretation of the estimate for the model of Lag 0? 
# What is the interpretation of the estimate for the model of Lag 1? 
##*****************************************************************## 


# 2d.vii Plot
# note that this is the same code 
# that we used in the quantile regression session
# to create the forest plots

fp.individual.lags  <- ggplot(
                              # defines what dataset ggplot will use
                              data = coeff.table,     
                              # aes() defines which variables the geoms will use   
                              aes( # defines variable for the x axis
                                x = ModelName,  
                                # defines the variable for the point along the y axis
                                y = coeff.rr,      
                                # defines the lower bound of the confidence interval
                                ymin = lci.rr,     
                                # define the upper bound of the confidence interval 
                                ymax = uci.rr)) +  
                              # creates a point (y) with line defined by ymin and ymax
                              geom_pointrange() +   
                              # creates lines with bars, i.e. here the CIs
                              geom_errorbar() +      
                              # add a dashed line at y=0
                              geom_hline(aes(yintercept = 1.0), lty = 2) +
                              # labels for axes
                              xlab("Model Name") +    
                              ylab(expression("RR per 10 "*mu*"g/m"^3*" PM"[2.5]~" (95% CI)"))

fp.individual.lags

##** Class Question 4 **##
# Do we have any issues in our interpretation of the coefficients?
##*****************************************************************## 


####***************************************
#### 3: Unconstrained Distributed Lags ####
####***************************************
# We will assume each lag has a linear dose-response relationship
# In this model we will adjust for 7-day mean temperature 
# and 7-day mean relative humidity 

# 3a Compute 7-day mean temp and RH

dta$meanTemp.7day <- rowMeans(dta[,15:21])
dta$meanRH.7day   <- rowMeans(dta[,22:28])

# 3b Construct the model 

mod.lag0_6 <- glm(dailyCVD ~ pm_Lag0 + pm_Lag1 + pm_Lag2 +
                  pm_Lag3 + pm_Lag4 + pm_Lag5 + pm_Lag6+
                  ns(meanTemp.7day, df = 4) + ns(meanRH.7day, df = 3) +
                  DayofWeek + ns(Date, df = 4*yr_num), 
                  family = "quasipoisson", data = dta)

# 3c Model Summary 
summary(mod.lag0_6)

# 3d Plot model 

# 3d.i Extract Coefficients 

coeff.table <- summary(mod.lag0_6)$coefficients[2:8, 1:2]

coeff.table <- as.data.frame(coeff.table)

# 3d.ii Set names for dataframe

names(coeff.table) <- c("coeff", "se")

# 3d.iii Compute confidence intervals 

coeff.table <- coeff.table %>% 
  mutate(lci = coeff - 1.96 * se, 
         uci = coeff + 1.96 * se)

# 3d.iv Exponeniate terms 
# since the link function is log() 
# when we exponantiate, we translate our coefficients 
# from log(RateRatio)'s
# to Rate Ratios 
# we will also multiply by 10 to compute the 
# the rate ratio per 10 ug/m3 increase in daily PM

coeff.table <- coeff.table %>% 
  mutate(fit.rr = exp(10*coeff), 
         lci.rr = exp(10*lci), 
         uci.rr = exp(10*uci))

# 3d.v Create Lag names 

coeff.table <- coeff.table %>% 
  mutate(Lag = c("Lag 0", "Lag 1", "Lag 2", "Lag 3", "Lag 4", "Lag 5", "Lag 6"))

# 3d.vi Plot

fp.unconstrained.dlm <- ggplot(data=coeff.table,     # defines what dataset we are using
                                  aes(x=Lag,            # defines variable for the x axis
                                      y=fit.rr,      # defines the variable for the point along the y axis
                                      ymin=lci.rr,     # defines the lower bound of the confidence interval
                                      ymax=uci.rr)) +  # define the upper bound of the confidence interval   
                                  geom_pointrange() +   # creates a point (y) with line defined by ymin and ymax        
                                  geom_errorbar()+      # creates lines with bars
                                  geom_hline(aes(yintercept=1), lty=2) + # add a dashed line at y=1 
                                  xlab("Model Name") +                   # labels for axes
                                  ylab(expression("RR per 10 "*mu*"g/m"^3*" PM"[2.5]~" (95% CI)"))

fp.unconstrained.dlm

# Let's clean up a little 
rm(mod.lag0, mod.lag1, mod.lag2, mod.lag3, mod.lag4, mod.lag5, mod.lag6, mod.lag0_6, 
    coeff.table, coeff.lag1, coeff.lag0, coeff.lag2, coeff.lag3, coeff.lag4, coeff.lag5, coeff.lag6,
   fp.individual.lags, fp.unconstrained.dlm)

####*******************************
#### 4: Linear Constrained DLM ####
####*******************************

# When we use the dlnm package, the first step is to 
# construct a crossbasis for each lagged variable. 
# The crossbasis is a specially organized matrix 
# with the parameters of the 2 dimensions(lag and exposure) 
# parallel to if you made an object with ns().
# When we construct the crossbasis, we define 
# the functional form of the relationship of the lags
# as well as the dose-response relationship 
# ie the constraints

# 4b Construct crossbasis for exposure

cb.lin.pm <- crossbasis(
  # the column with the exposure 
  x=dta$dailyPM,         
  # The number of lags
  lag=7,    
  # the functional form of the constraint on the exposure dimension
  argvar=list(fun="lin"), 
  # the functional form of the constraint on the lag dimension
  arglag=list(fun="ns", df = 4)) 

##** Class Question 5 **##
# What do we assume when we set lag to 7? 
# What does fun="lin" mean? 
# What does fun="ns" mean? 
##*****************************************************************## 


# The warning refers to this update:
# "An important change [to crossbasis() ]
# is that the knots for the spline functions or 
# cut-offs for strata in the lag dimension are now
# placed at equally-spaced percentiles if not specified, 
# differently from the default in previous versions."


# 4c Check crossbasis
# I always double check that my crossbasis follows my design 
# ie - correct number of lags, etc
# remember, summary() is a generic function, 
# and recognizes that cb.lin.pm is a crossbasis object, 
# not a model

summary(cb.lin.pm)

##** Class Question 6 **##
# How many lags does our crossbasis cover? 
# How many knots are in the natural spline for lags?
##*****************************************************************## 


# 4d Construct crossbasis for temperature

cb.ns.temp <- crossbasis(
  # the column with the exposure 
  dta$dailyTemp,         
  # The number of lags
  lag=7,    
  # the functional form of the dose-response curve
  argvar=list(fun="ns", df = 4), 
  # the functional form of the lags
  arglag=list(fun="ns", df = 4)) 

##** Class Question 6 **##
# Why did we use argvar=list(fun="ns"df=4)?
##*****************************************************************## 


summary(cb.ns.temp)
# 4e Construct crossbasis for relative humidity

cb.ns.rh <- crossbasis(
  # the column with the exposure 
  dta$dailyRH,         
  # The number of lags
  lag=7,    
  # the functional form of the dose-response curve
  argvar=list(fun="ns", df = 4), 
  # the functional form of the lags
  arglag=list(fun="ns", df = 4)) 

# 4f Fit distributed lag model 
# note here that we are using the same 
# glm model as with the time-series analysis 
# the only thing that has changed is 
# that we are using the crossbasis terms in the formula

mod.lin <- glm(dailyCVD ~   # outcome
                 cb.lin.pm +  # lagged, linear term for exposure
                 cb.ns.temp + # lagged, nonlinear term for Temperature
                 cb.ns.rh +   # lagged, nonlinear term for RH
                 DayofWeek +  # Same-day categorical variable for DoW
                 ns(Date, df = 4*yr_num), #Same-day nonlinear term for secular trend
               family = "quasipoisson",   # distribution family
               data = dta)

# 4g Extract associations 
# we can use crosspred() to estimate the association between 
# a variable and the outcome 
# at various values and lags
# based on the model we fit
# crosspred() acts in a similar role as
# pred() in previous labs

pred.lin.pm <- crosspred(
  # the exposure crossbasis
  basis = cb.lin.pm,   
  # the model
  model=mod.lin, 
  # compute the estimated association for 
  # each integer value of PM2.5
  # between 0 and 55
  at = 0:55, 
  # estimate association along 
  # lags in increments of 0.2 
  # using the natural splines 
  # to interpolate between days
  bylag = 0.2,  
  # also compute cumulative association
  cumul=TRUE)

# 4h Plot results 

# 4h.i Association at each lag
png(paste0("~/Desktop/iagme.png"), width = 500, height =400)
par(mar = c(5,7,4,2) + 0.1)
plot(pred.lin.pm, 
     var=10,    # units of change of independent variable
     col="red", 
     ylab=" ", 
     ci.arg=list(density=15,lwd=2),
     main=expression("Association for a 10-unit Increase in PM"[2.5]), 
     yaxp = c(0.990, 1.015, 10), las =1)
title(ylab = "Rate Ratio", line = 4.5)
dev.off()
##** Class Question 7 **##
# How do we interpret this plot? 
##*****************************************************************## 


# 4k Extract Coefficients 

# 4k.i Extract coefficient fit  
fit.table.lin <- as.data.frame(pred.lin.pm$matRRfit)  
colnames(fit.table.lin) <- paste0("fit.rr_", colnames(fit.table.lin))
fit.table.lin <- fit.table.lin %>%   
  mutate(PM2.5 = as.numeric(row.names(fit.table.lin)))

# 4k.ii Extract 95% CI  
lci.table.lin <- as.data.frame(pred.lin.pm$matRRlow)  
colnames(lci.table.lin) <- paste0("lci.rr_", colnames(lci.table.lin))

uci.table.lin <- as.data.frame(pred.lin.pm$matRRhigh)  
colnames(uci.table.lin) <- paste0("uci.rr_", colnames(uci.table.lin))

# 4k.iii Combine fit and se 
pred.table.lin <- bind_cols(fit.table.lin, lci.table.lin, uci.table.lin)

# 4k.iv Review Table 
Mini.Pred.Table.lin <- pred.table.lin %>% 
  dplyr::select(PM2.5, fit.rr_lag0, lci.rr_lag0, uci.rr_lag0,
                fit.rr_lag1, lci.rr_lag1, uci.rr_lag1, 
                fit.rr_lag2, lci.rr_lag2, uci.rr_lag2)
View(Mini.Pred.Table.lin)

##** Class Question 8 **##
# What is each row? 
# What is each colum?
##*****************************************************************## 


# 4k.v Present quantative results 

CoeffA <- paste0(round(Mini.Pred.Table.lin$fit.rr_lag0[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.lin$lci.rr_lag0[11],4), ", ", 
                 round(Mini.Pred.Table.lin$uci.rr_lag0[11],4), ")")
CoeffA

##** Class Question 9 **##
# What is the interpretation of coefficient A?
##*****************************************************************## 


CoeffB <- paste0(round(Mini.Pred.Table.lin$fit.rr_lag1[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.lin$lci.rr_lag1[11],4), ", ", 
                 round(Mini.Pred.Table.lin$uci.rr_lag1[11],4), ")")
CoeffB

##** Class Question 10 **##
# What is the interpretation of this coefficient?
# Where is CoeffA in the Plot? CoeffB?
##*****************************************************************## 


# 4l Cumulative association

# 4l.i Plot culumative association
plot(pred.lin.pm, "slices", var=10, col="orange", cumul=TRUE, 
     ylab=expression("Cumulative RR"),
     main=expression("Cumulative association with a 10-unit increase in PM"[2.5]))

##** Class Question 11 **##
# What does this plot tell us?
# What is the cumulative association?
##*****************************************************************## 


# 4l.ii Extract coefficient fit for cumulative association
fit.table.cumu.lin <- as.data.frame(pred.lin.pm$cumRRfit)  
colnames(fit.table.cumu.lin) <- paste0("cumu.fit.rr_", colnames(fit.table.cumu.lin))
fit.table.cumu.lin <- fit.table.cumu.lin %>%   mutate(PM2.5 = as.numeric(row.names(fit.table.cumu.lin)))

# 4l.iii Extract 95% CI  
lci.table.cumu.lin <- as.data.frame(pred.lin.pm$cumRRlow)  
colnames(lci.table.cumu.lin) <- paste0("cumu.lci.rr_", colnames(lci.table.cumu.lin))

uci.table.cumu.lin <- as.data.frame(pred.lin.pm$cumRRhigh)  
colnames(uci.table.cumu.lin) <- paste0("cumu.uci.rr_", colnames(uci.table.cumu.lin))

# 4l.i Combine fit and se 
 
pred.table.cumu.lin <- bind_cols(fit.table.cumu.lin, lci.table.cumu.lin, uci.table.cumu.lin)

# 4k.iv Review Table 
Mini.Pred.Table.Cumu.lin <- pred.table.cumu.lin %>% 
      dplyr::select(PM2.5, cumu.fit.rr_lag0, cumu.lci.rr_lag0, cumu.uci.rr_lag0,
                    cumu.fit.rr_lag1, cumu.lci.rr_lag1, cumu.uci.rr_lag1, 
                    cumu.fit.rr_lag2, cumu.lci.rr_lag2, cumu.uci.rr_lag2)
View(Mini.Pred.Table.Cumu.lin)

# 4k.v Present quantative results 

CoeffC <- paste0(round(Mini.Pred.Table.Cumu.lin$cumu.fit.rr_lag0[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.Cumu.lin$cumu.lci.rr_lag0[11],4), ", ", 
                 round(Mini.Pred.Table.Cumu.lin$cumu.uci.rr_lag0[11],4), ")")
CoeffC

##** Class Question 12 **##
# What is the interpretation of this coefficient?
##*****************************************************************## 


CoeffD <- paste0(round(Mini.Pred.Table.Cumu.lin$cumu.fit.rr_lag1[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.Cumu.lin$cumu.lci.rr_lag1[11],4), ", ", 
                 round(Mini.Pred.Table.Cumu.lin$cumu.uci.rr_lag1[11],4), ")")
CoeffD

##** Class Question 13 **##
# What is the interpretation of this coefficient?
##*****************************************************************## 


####***********************************
#### 5: Nonlinear Constrained DLNM ####
####***********************************

# We will create a new crossbasis for PM2.5
# and use the previous crossbases for Temperature and RH 
# since they were already non-linear

# 5a Construct crossbasis for exposure

cb.ns.pm <- crossbasis(
  # the column with the exposure 
  dta$dailyPM,         
  # The number of lags
  lag = 7,    
  # the functional form of the dose-response curve
  argvar = list(fun="ns", df = 4), 
  # the functional form of the lags
  arglag = list(fun="ns", df = 4)) 

# 5b Check crossbasis

summary(cb.ns.pm)

# 5c Fit Distributed Lag Nonlinear Model 

mod.ns <- glm(dailyCVD ~   # outcome
                cb.ns.pm +    # lagged, linear term for exposure
                cb.ns.temp +  # lagged, nonlinear term for Temp
                cb.ns.rh +    # lagged, nonlinear term for RH
                DayofWeek + # Same-day categorical variable for DoW
                ns(Date, df = 4*yr_num),   #Same-day nonlinear term for secular trend
              family = "quasipoisson",   # distribution family
              data = dta)

# 5d Extract predictions 

pred.ns.pm <- crosspred(
  # the exposure crossbasis
  cb.ns.pm,   
  # the model
  mod.ns, 
  # compute the estimated association for 
  # each integer value of PM2.5
  # between 0 and 55
  at = 0:55, 
  # estimates association along 
  # lags in increments of 0.2 
  # using the natural splines 
  # to interpolate between days
  bylag = 0.2,  
  # Reference exposure is 0 ug/m3
  cen = 0,
  # also compute cumulative associations
  cumul=TRUE)

# 5e 3d Plot 

plot(pred.ns.pm, 
     xlab="\nPM2.5", zlab="\nRR", ylab="\nLag", 
     theta=40, phi=30, lphi=30,
     main=expression("3D graph of PM"[2.5]*" Association"))

# we can adjust the angle of the plot to see different perspectives 
plot(pred.ns.pm, 
     xlab="\nPM2.5", zlab="\nRR", ylab="\nLag", 
     theta=220, phi=30, lphi=30,
     main=expression("3D graph of PM"[2.5]*" Association"))

plot(pred.ns.pm, 
     xlab="\nPM2.5", zlab="\nRR", ylab="\nLag",
     theta=100, phi=30, lphi=30,
     main=expression("3D graph of PM"[2.5]*" Association"))

# 5f Extract Coefficients 

# 5f.i Extract coefficient fit  

fit.table.ns <- as.data.frame(pred.ns.pm$matRRfit)  
colnames(fit.table.ns) <- paste0("fit.rr_", colnames(fit.table.ns))
fit.table.ns <- fit.table.ns %>%   mutate(PM2.5 = as.numeric(row.names(fit.table.ns)))

# 5f.ii Extract 95% CI  

lci.table.ns <- as.data.frame(pred.ns.pm$matRRlow)  
colnames(lci.table.ns) <- paste0("lci.rr_", colnames(lci.table.ns))

uci.table.ns <- as.data.frame(pred.ns.pm$matRRhigh)  
colnames(uci.table.ns) <- paste0("uci.rr_", colnames(uci.table.ns))

# 5f.iii Combine fit and se 

pred.table.ns <- bind_cols(fit.table.ns, lci.table.ns, uci.table.ns)

# 5f.iv Review Table 

Mini.Pred.Table.ns <- pred.table.ns %>% 
  dplyr::select(PM2.5, fit.rr_lag0, lci.rr_lag0, uci.rr_lag0,
         fit.rr_lag1, lci.rr_lag1, uci.rr_lag1, 
         fit.rr_lag2, lci.rr_lag2, uci.rr_lag2)
View(Mini.Pred.Table.ns)

##** Class Question 14 **##
# What are the rows? The columns?
##*****************************************************************## 


# 5f.v Present quantative results 

CoeffE <- paste0(round(Mini.Pred.Table.ns$fit.rr_lag0[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.ns$lci.rr_lag0[11],4), ", ", 
                 round(Mini.Pred.Table.ns$uci.rr_lag0[11],4), ")")
CoeffE

##** Class Question 15 **##
# What is the interpretation of this coefficient?
# What is the difference between this coefficient and the coefficient for the 
# model with a linear exposure-response relationship for PM2.5?
##*****************************************************************## 


CoeffF <- paste0(round(Mini.Pred.Table.ns$fit.rr_lag1[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.ns$lci.rr_lag1[11],4), ", ", 
                 round(Mini.Pred.Table.ns$uci.rr_lag1[11],4), ")")
CoeffF

##** Class Question 16 **##
# What is the interpretation of this coefficient?
##*****************************************************************## 


# 5h Plot Association for One Lag 

ggplot(pred.table.ns) + 
  geom_line(aes(PM2.5, fit.rr_lag0), color= "red") + 
  geom_ribbon(aes(x= PM2.5, ymin= lci.rr_lag0,  ymax = uci.rr_lag0), 
              fill = "red",  alpha  = 0.20)+
  geom_hline(yintercept = 1) + 
  labs(x= "PM2.5 on Lag 0", y = "Rate Ratio of CVD Hospitalization")

##** Class Question 17 **##
# How would we describe this relationship? 
# Which effect estimate from before is represented on this plot? Where? 
##*****************************************************************## 


ggplot(pred.table.ns) + 
  geom_line(aes(PM2.5, fit.rr_lag1), color= "pink") + 
  geom_ribbon(aes(x= PM2.5, ymin= lci.rr_lag1,  ymax = uci.rr_lag1), fill = "pink",  alpha  = 0.20)+
  geom_hline(yintercept = 1)

ggplot(pred.table.ns) + 
  geom_line(aes(PM2.5, fit.rr_lag2), color= "purple") + 
  geom_ribbon(aes(x= PM2.5, ymin= lci.rr_lag3,  ymax = uci.rr_lag2), fill = "purple",  alpha  = 0.20)+
  geom_hline(yintercept = 1)

ggplot(pred.table.ns) + 
  geom_line(aes(PM2.5, fit.rr_lag0), color= "red") + 
  geom_line(aes(PM2.5, fit.rr_lag1), color= "pink") + 
  geom_line(aes(PM2.5, fit.rr_lag2), color= "purple") + 
  geom_line(aes(PM2.5, fit.rr_lag3), color= "blue") + 
  geom_hline(yintercept = 1)

# Plot effect across lags
plot(pred.ns.pm, 
     var=10,    # units of change of independent variable
     col="red", 
     ylab=expression("RR for a 10-unit increase in PM"[2.5]), 
     ci.arg=list(density=15,lwd=2),
     main=expression("Association with a 10-unit increase in PM"[2.5]))

# 5g Cumulative association

# 5g.i Plot culumative association

plot(pred.ns.pm, "slices", var=10, col="orange", cumul=TRUE, ylab="Cumulative RR",
     main=expression("Cumulative association with a 10-unit increase in PM"[2.5]))

##** Class Question 18 **##
# What does the cumulative association mean for this model?
##*****************************************************************## 


# 5g.ii Extract coefficient fit for cumulative association

fit.table.cumu.ns <- as.data.frame(pred.ns.pm$cumRRfit)  
colnames(fit.table.cumu.ns) <- paste0("cumu.fit_", colnames(fit.table.cumu.ns))
fit.table.cumu.ns <- fit.table.cumu.ns %>%   mutate(PM2.5 = as.numeric(row.names(fit.table.cumu.ns)))

# 5g.iii Extract 95% CI 

lci.table.cumu.ns <- as.data.frame(pred.ns.pm$cumRRlow)  
colnames(lci.table.cumu.ns) <- paste0("cumu.lci_", colnames(lci.table.cumu.ns))

uci.table.cumu.ns <- as.data.frame(pred.ns.pm$cumRRhigh)  
colnames(uci.table.cumu.ns) <- paste0("cumu.uci_", colnames(uci.table.cumu.ns))

# 5g.i Combine fit and se 

pred.table.cumu.ns <- bind_cols(fit.table.cumu.ns, lci.table.cumu.ns, uci.table.cumu.ns) %>% 
  rename(cumu.fit.rr_lag0 = cumu.fit_lag0, 
         cumu.lci.rr_lag0 = cumu.lci_lag0, 
         cumu.uci.rr_lag0 = cumu.uci_lag0,
         cumu.fit.rr_lag1 = cumu.fit_lag1, 
         cumu.lci.rr_lag1 = cumu.lci_lag1, 
         cumu.uci.rr_lag1 = cumu.uci_lag1,
         cumu.fit.rr_lag2 = cumu.fit_lag2, 
         cumu.lci.rr_lag2 = cumu.lci_lag2, 
         cumu.uci.rr_lag2 = cumu.uci_lag2)

# 5g.iv Review Table 

Mini.Pred.Table.Cumu.ns <- pred.table.cumu.ns %>% 
  # dplyr::select(PM2.5, contains(c("lag0", "lag1", "lag2"))
  dplyr::select(PM2.5, cumu.fit.rr_lag0, cumu.lci.rr_lag0, cumu.uci.rr_lag0,
         cumu.fit.rr_lag1, cumu.lci.rr_lag1, cumu.uci.rr_lag1, 
         cumu.fit.rr_lag2, cumu.lci.rr_lag2, cumu.uci.rr_lag2)
View(Mini.Pred.Table.Cumu.ns)

# 5g.v Present quantative results 

CoeffG <- paste0(round(Mini.Pred.Table.Cumu.ns$cumu.fit.rr_lag0[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.Cumu.ns$cumu.lci.rr_lag0[11],4), ", ", 
                 round(Mini.Pred.Table.Cumu.ns$cumu.uci.rr_lag0[11],4), ")")
CoeffG

##** Class Question 19 **##
# What is the interpretation of this coefficient?
##*****************************************************************## 


# A 10 ug/m3 increase in daily mean PM2.5 for two days, 
# while holding PM2.5 of the previous 6 days constant, 
# is associated with a 1.0099 times (95% CI: 1.0014, 1.0185)
# increase in the rate of CVD hospitalizations, on the following day.


CoeffH <- paste0(round(Mini.Pred.Table.Cumu.ns$cumu.fit.rr_lag1[11],4), "(95% CI: ",
                 round(Mini.Pred.Table.Cumu.ns$cumu.lci.rr_lag1[11],4), ", ", 
                 round(Mini.Pred.Table.Cumu.ns$cumu.uci.rr_lag1[11],4), ")")
CoeffH

##** Class Question 20 **##
# What is the interpretation of this coefficient?
##*****************************************************************## 


# 5g.i Plot culumative association

plot(pred.ns.pm, "slices", var=10, col="orange", cumul=TRUE, ylab="Cumulative RR",
     main=expression("Cumulative association with a 10-unit increase in PM"[2.5]))

####*********************************************
#### Footnote: Too Many df for Lag Dimension ####
####*********************************************

# A Create confounder crossbases
# A1 Construct crossbasis for temperature

cb.ns.temp <- crossbasis(dta$dailyTemp, lag=7, argvar=list(fun="ns", df = 4),  
                         arglag=list(fun="ns", df = 4)) 

# A2 Construct crossbasis for relative humidity

cb.ns.rh <- crossbasis(dta$dailyRH, lag=7, argvar=list(fun="ns", df = 4),
                       arglag=list(fun="ns", df = 4)) 

# B: Construct model with 8 df for lag 
# B1 Construct crossbasis for exposure

cb.8df.pm <- crossbasis(dta$dailyPM, lag=7, argvar=list(fun="lin"), 
                        arglag=list(fun="ns", df = 8)) 

# B2 Distributed Lag Model 

mod.8df <- glm(dailyCVD ~ cb.8df.pm + cb.ns.temp + cb.ns.rh + DayofWeek + ns(Date, df = 4*yr_num),  
               family = "quasipoisson", data = dta)

# B3 Extract associations 

pred.8df.pm <- crosspred(cb.8df.pm, mod.8df, at = 0:55,  bylag = 0.2,  cumul=TRUE)

plot(pred.8df.pm, var=10, col="red", ylab="RR", 
     ci.arg=list(density=15,lwd=2),
     main="8df for Lag Dimension")

####**************************************************************
#### FootNote: Examining Different Lag DF for Linear ER Model ####
####**************************************************************

# A Create confounder crossbases
# A1 Construct crossbasis for temperature

cb.ns.temp <- crossbasis(dta$dailyTemp, lag=7, argvar=list(fun="ns", df = 4),  arglag=list(fun="ns", df = 4)) 

# A2 Construct crossbasis for relative humidity

cb.ns.rh <- crossbasis(dta$dailyRH, lag=7, argvar=list(fun="ns", df = 4), arglag=list(fun="ns", df = 4)) 

# B: Construct model with 6 df for lag 
# B1 Construct crossbasis for exposure

cb.6df.pm <- crossbasis(dta$dailyPM, lag=7, argvar=list(fun="lin"), arglag=list(fun="ns", df = 6)) 

# B2 Distributed Lag Model 

mod.6df <- glm(dailyCVD ~ cb.6df.pm + cb.ns.temp + cb.ns.rh + DayofWeek + ns(Date, df = 4*yr_num),  
               family = "quasipoisson", data = dta)

# B3 Extract associations 

pred.6df.pm <- crosspred(cb.6df.pm, mod.6df, at = 0:55,  bylag = 0.2,  cumul=TRUE)

# C: Construct model with 5 df for lag 
# C1 Construct crossbasis for exposure

cb.5df.pm <- crossbasis(dta$dailyPM, lag=7, argvar=list(fun="lin"), arglag=list(fun="ns", df = 5)) 

# C2 Distributed Lag Model 

mod.5df <- glm(dailyCVD ~ cb.5df.pm + cb.ns.temp + cb.ns.rh + DayofWeek + ns(Date, df = 4*yr_num),  
               family = "quasipoisson", data = dta)

# C3 Extract associations 

pred.5df.pm <- crosspred(cb.5df.pm, mod.5df, at = 0:55,  bylag = 0.2,  cumul=TRUE)

# C: Construct model with 5 df for lag 
# C1 Construct crossbasis for exposure

cb.4df.pm <- crossbasis(dta$dailyPM, lag=7, argvar=list(fun="lin"), arglag=list(fun="ns", df = 4)) 

# C2 Distributed Lag Model 

mod.4df <- glm(dailyCVD ~ cb.4df.pm + cb.ns.temp + cb.ns.rh + DayofWeek + ns(Date, df = 4*yr_num),  
               family = "quasipoisson", data = dta)

# C3 Extract associations 

pred.4df.pm <- crosspred(cb.4df.pm, mod.4df, at = 0:55,  bylag = 0.2,  cumul=TRUE)


# E: Construct model with 3 df for lag 
# E1 Construct crossbasis for exposure

cb.3df.pm <- crossbasis(dta$dailyPM, lag=7, argvar=list(fun="lin"), arglag=list(fun="ns", df = 3)) 

# E2 Distributed Lag Model 

mod.3df <- glm(dailyCVD ~ cb.3df.pm + cb.ns.temp + cb.ns.rh + DayofWeek + ns(Date, df = 4*yr_num),  
               family = "quasipoisson", data = dta)

# E3 Extract associations 

pred.3df.pm <- crosspred(cb.3df.pm, mod.3df, at = 0:55,  bylag = 0.2,  cumul=TRUE)

# F Plot the 4 models 
#pdf(paste0(DataFolder, "Df_of_Lag_for_linear_PM.pdf"))

par(mfrow = c(2,2))

plot(pred.6df.pm, var=10, col="red", ylab="RR", 
     ci.arg=list(density=15,lwd=2),
     main="6 df for Lag")

plot(pred.5df.pm, var=10, col="red", ylab="RR", 
     ci.arg=list(density=15,lwd=2),
     main="5 df for Lag")

plot(pred.4df.pm, var=10, col="red", ylab="RR", 
     ci.arg=list(density=15,lwd=2),
     main="4 df for Lag")

plot(pred.3df.pm, var=10, col="red", ylab="RR", 
     ci.arg=list(density=15,lwd=2),
     main="3 df for Lag")

#dev.off()



