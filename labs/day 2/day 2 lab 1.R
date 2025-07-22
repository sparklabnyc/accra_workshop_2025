# Session 03: Non-Linearity 
# Marianthi-Anna Kioumourtzoglou
# Sebastian Rowland
# Advanced Analytic Methods for Environmental Epidemiology

####***********************
#### Table of Contents ####
####***********************

## Pre-Class Material ##
# 0: Preparation 
# 1: Define Research Question
# 2: Load Data 
# 3: Quadratic Term

## Live Session Material ## 
# 4: Define Research Question
# 5: Piecewise Linear Spline Term 
# 6: Natural Spline Term 
# 7: Penalized Spline Term 
# 8: Group Exercises 

## Bonus Material ## 
# Footnote: Plotting All the Models Together
# Footnote: Cubic Term 
# Footnote: Multiple Plots

####************************
#### Pre-Class Material ####
####************************

####********************
#### 0: Preparation ####
####********************

# 0a Install packages 

# mgcv allows for quick modeling of generalized additive models 
# install.packages("mgcv")

# splines gives additional spline options
# install.packages("splines")

# 0b Load packages
library(readr)
library(dplyr) 
library(ggplot2)
library(mgcv)
library(splines)

#0c Declare folder paths

ProjectFolder <- here::here("data")

# ProjectFolder <- paste0("/Users/marianthi_anna/Dropbox/AAMEHS/Spring2021/", 
#                          "A2_Labs/Lab03_Nonlinearity/")

####*********************************
#### 1: Define Research Question ####
####*********************************

# In 2010, was county-average BMI associated with county-average annual PM2.5? 
# What is the shape of this relationship?
# Is the association constant across values of PM2.5?

####******************
#### 2: Load Data ####
####******************

# 2a Load data 

df <- read_csv(here::here(ProjectFolder, "2010_County_Data.csv"))

# 2b Keep only complete cases 

df <- df %>% filter(complete.cases(df))

# alternative command 
# df <- na.omit(df)

####*******************************************
# Variable Codebook 
# FIPS: unique identifier for each county 
## from the Behavioral RIsk Factor Surveillance System Survey 
##      of the Centers for Disease Control and Prevention
# AveBMI: average Body Mass Index (BMI) for each county in 2010
#         BMI is calculated from body weight / square of height (units are kg/m2)
## from the Air Quality System 
##      of the US Environmental Protection Agency 
# AvePM: annual ambient PM2.5, based on inverse-distance weighted average of EPA monitors
## from 2010 American Community Survey 
##      of the US Census Bureau
# NumTot: total population in county
# PerBlack, PerLatinx, PerAsam, PerWhite: racial composition 
# MedHInc: median household income 
# MedHVal: median household value among homeowners 
# FemaleUnemp, MaleUnemp: unemployment rate by sex
# unemployment has a specific definition, not just 'not working', must be seeking work. 
# LTHS: percentage of population over 18 with less than high school education 
## from the National Oceanic and Atmostpheric Agency
# ClimateRegion: categorical variable of areas with similar climates
####*******************************************

####***********************
#### 3: Quadratic Term ####
####***********************

# 3a Create model 
# we can create new terms within the formula arguement
# using the I() command 
# here we are creating a variable for squared PM2.5 
# our regression model looks like: 
# E[Y] = b0 + b1*PM2.5 + b2*PM2.5^2 + B3*FemaleUnemp....

mod.quad <- lm(AveBMI ~ AvePM + I(AvePM^2) + FemaleUnemp + MaleUnemp + LTHS + 
                 MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm + ClimateRegion, 
               data = df)

# 3b Model Summary 

summary(mod.quad)

##** PreClass Task: Calculate Effect Estimate **##
# A) What is the interpretation of Coefficient 3? 
# B) How would I estimate the change in expected AveBMI 
#       for a 6 to 10 ug/m3 increase in PM2.5 concentration?
# hint: How many coefficients do we need to calculated the effect estimate? 
#       When we calculated effect estimates in the presence of effect 
#       modification (Lab 1), how did we do it? 
##*****************************************************************## 

# 3c Plot Exposure-Response Curve

# 3c.i Construct predictions based on the model
# here predict() computes expected AveBMI for the observation 
# if that observation had the observed value of that variable 
# and the mean value for all other variables 
# These predictions are also centered around the mean AveBMI.
# But they are not scaled

predBMI.quad <- predict(mod.quad, se.fit = TRUE, type = "terms" )

# 3c.ii Convert to dataframe 

predBMI.quad <- as.data.frame(predBMI.quad)

# 3c.iii Combine predictions for linear and quadratic terms

predBMI.quad <- predBMI.quad %>% 
  mutate( pred = fit.AvePM + fit.I.AvePM.2.)

# 3c.iv Keep only variables we need 

predBMI.quad <- predBMI.quad %>% dplyr::select(pred)

# 3c.v Combine with data 

predBMI.quad <- predBMI.quad %>% bind_cols(df)

# 3c.vi Uncenter the predictions - add mean AveBMI 

predBMI.quad <- predBMI.quad %>% mutate(predBMI = pred + mean(AveBMI))

# 3c.vii Plot

ggplot(predBMI.quad, aes(x = AvePM)) + 
  geom_line(aes(y = predBMI)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted Average BMI") + 
  ylim(26, 29) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

# 3c.viii Uncentered Plot
ggplot(predBMI.quad, aes(x = AvePM)) + 
  geom_line(aes(y = pred)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Change in Expected \nAverage BMI") + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

# 3d Assess model fit 

# Did adding the quadratic term improve the fit of our model? 
# we can compare the fit of the quadratic model 
# with the fit of the more simple linear model 

# 3d.i Create linear model 

mod.lin <- lm(AveBMI ~ AvePM + FemaleUnemp + MaleUnemp + LTHS + 
                MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm + ClimateRegion, 
              data = df)

# 3d.ii Likelihood Ratio Test (LRT)
# anova() is another generic command, like summary or plot
# If anova() detects that its inputs are two nested models 
# then anova() will conduct a likelihood ratio test 

anova(mod.quad, mod.lin)

##** PreClass Task: Calculate Effect Estimate **##
# Based on the LRT, did adding the quadratic term improve the model fit? 
# Which model would you choose to present? Why? 
##*****************************************************************## 

# 3e Plot both models  
# 3e.i Extract predictions from linear model 
predBMI.lin <- predict(mod.lin, se.fit = TRUE, type = "terms" )
predBMI.lin <- as.data.frame(predBMI.lin)
predBMI.lin <- predBMI.lin %>% mutate(pred = fit.AvePM)
predBMI.lin <- predBMI.lin %>% dplyr::select(pred)
predBMI.lin <- predBMI.lin %>% bind_cols(df)
predBMI.lin <- predBMI.lin %>% mutate(predBMI = pred + mean(AveBMI))

# 3e.ii Create a column with the model name 
# we will use this to keep track of the models once we combine them
predBMI.lin  <- predBMI.lin    %>% mutate(ModelName = "Linear Term")
predBMI.quad <- predBMI.quad   %>% mutate(ModelName = "Quadratic Term")

# 3e.iii Combine predictions from both models 
predBMI.tot <- bind_rows(predBMI.lin,
                         predBMI.quad)

# e.iv Order the variables as factors 
# This makes the legend easier to follow
predBMI.tot <- predBMI.tot %>% 
  mutate(ModelName = factor(ModelName, 
                            levels = c("Linear Term", "Quadratic Term", "Piecewise Linear Spline", 
                                       "Natural Spline 3 df", "Penalized Spline")))

# 3e.v Plot 
# in this plot, 
# each point is an observation (one point per county)
# and the two lines are the estimated exposure-response curves of the two models
all.models.plot <-  ggplot(predBMI.tot, aes(AvePM)) + 
  geom_line(aes(y = predBMI, color = ModelName)) + 
  geom_point(aes(y = AveBMI)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI") + 
  ylim(26.5, 28.5) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

all.models.plot 

##** PreClass Task: Calculate Effect Estimate **##
# Review this plot. 
# Do the models yield overall similar results? 
# Where do the models disagree? 
##*****************************************************************## 

####***************************
#### Live Session Material ####
####***************************

####*********************************
#### 4: Define Research Question ####
####*********************************

# In 2010, was county-average BMI associated with county-average annual PM2.5? 
# What is the shape of this relationship?

####*************************************
#### 5: Piecewise Linear Spline Term ####
####*************************************

# Another method for modeling non-linearity 
# is to create multiple individual linear terms, "splines"

# 5a Create spline term 
# the knot is at 12 
# for observations with pm =< 12, pm.high is 0
# for observations with pm > 12, pm.high is pm-12
# This calcuation ensures that the exposure-response curve is continuous
df <- df %>% mutate( AvePM.high = (AvePM-12) * (AvePM >12))

# 5a.ii Review AvePM.high
df1 <- df %>% mutate( AvePM12 = AvePM-12,
                      AvePM.gt12 = (AvePM >12),
                      AvePM.gt12num  = 1* (AvePM >12)) %>% 
  dplyr::select(AvePM, AvePM.gt12, AvePM.gt12num, AvePM.high)

head(df1)

# 5b Create model with piecewise linear spline term 

mod.pls <- lm(AveBMI ~ AvePM + AvePM.high + FemaleUnemp + MaleUnemp + LTHS + 
                MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm + ClimateRegion, 
              data = df)

# 5c Review model 

summary(mod.pls)

##** Class Question 1 **##
# What is the interpretation of the coefficient for AvePM.high?
# What would a positive coeffient mean? 
# What about a negative coefficient?
##*****************************************************************## 

# 5d Plot Exposure-Response Relationship

# 5d.i Create predictions

predBMI.pls <- predict(mod.pls, se.fit = TRUE, type = "terms" )

# 5d.ii Convert to dataframe 

predBMI.pls <- as.data.frame(predBMI.pls)

# 5d.iii Combine predictions from the two spline terms 

predBMI.pls <- predBMI.pls %>% 
  mutate(pred.centered = fit.AvePM + fit.AvePM.high)

# 5d.iv Keep only variables we need 

predBMI.pls <- predBMI.pls %>% dplyr::select(pred.centered)

# 5d.v Combine with data 

predBMI.pls <- predBMI.pls %>% bind_cols(df)

# 5d.vi Uncenter the data - add mean BMI 

predBMI.pls <- predBMI.pls %>% mutate(predBMI = pred.centered + mean(AveBMI))

# 5d.vii Plot

ggplot(predBMI.pls, aes(AvePM)) + 
  geom_line(aes(y = predBMI)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted Average BMI") + 
  ylim(26, 29) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

# 5e Plot multiple models 

# 5e.i Create a column with the model name 

predBMI.pls  <- predBMI.pls %>% mutate(ModelName = "Piecewise Linear Spline")

# 5e.ii Combine predictions from both models

predBMI.tot <- bind_rows(predBMI.tot,
                         predBMI.pls)

# 5e.iii Order the variables as factors 
# This makes the legend easier to follow

predBMI.tot <- predBMI.tot %>% 
  mutate(ModelName = factor(ModelName, 
                            levels = c("Linear Term", "Quadratic Term", "Piecewise Linear Spline", 
                                       "Natural Spline 3 df", "Penalized Spline")))

# 5e.iv Plot 

all.models.plot <-  ggplot(predBMI.tot, aes(AvePM)) + 
  geom_line(aes(y = predBMI, color = ModelName)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI") + 
  ylim(26.5, 28.5) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

all.models.plot 

####****************************
#### 6: Natural Spline Term ####
####****************************

# 6a Create model 
# we can constuct natural splines with the ns() command 
# we have to define the degrees of freedom with df arguement

ns.PM <- ns(df$AvePM, df = 3)

# we can also create the natural spline within the lm() command
mod.ns.3 <- lm(AveBMI ~ ns(AvePM, df = 3) + FemaleUnemp + MaleUnemp + LTHS + 
                 MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm +
                 ClimateRegion, data = df)

# 6b Model Summary 

summary(mod.ns.3)

# 6c Plot Exposure-Response Relationship

# 6c.i Construct predictions based on the model

predBMI.ns.3 <- predict(mod.ns.3, se.fit = TRUE, type = "terms" )
# head(predBMI.ns.3$fit)[,1]
# head(mod.ns.3$fit)
# 6c.ii Convert to dataframe 

predBMI.ns.3 <- as.data.frame(predBMI.ns.3)
# 6c.iii Rename predictions and standard errors
# column has different names since it is one term 

predBMI.ns.3 <- predBMI.ns.3 %>% 
  mutate( pred.centered = fit.ns.AvePM..df...3.,
          se = se.fit.ns.AvePM..df...3.)

# 6c.iv Compute 95% confidence intervals 

predBMI.ns.3 <- predBMI.ns.3 %>% 
  mutate( lci.centered = pred.centered - 1.96*se,
          uci.centered = pred.centered + 1.96*se)

# 6c.v Keep only variables we need

predBMI.ns.3 <- predBMI.ns.3 %>% 
  dplyr::select(pred.centered, se, lci.centered, uci.centered)

# 6c.vi Combine with data 

predBMI.ns.3 <- predBMI.ns.3 %>% bind_cols(df)

# 6c.vii Uncenter 

predBMI.ns.3 <- predBMI.ns.3 %>% mutate(predBMI = pred.centered + mean(AveBMI),
                                        lciBMI = lci.centered + mean(AveBMI),
                                        uciBMI = uci.centered + mean(AveBMI))
# 6c.viii Plot

# 6c.i Construct predictions based on the model

predBMI.ns.2 <- predict(mod.ns.2, se.fit = TRUE, type = "terms" )
# head(predBMI.ns.3$fit)[,1]
# head(mod.ns.3$fit)
# 6c.ii Convert to dataframe 

predBMI.ns.2 <- as.data.frame(predBMI.ns.2)
# 6c.iii Rename predictions and standard errors
# column has different names since it is one term 

predBMI.ns.2 <- predBMI.ns.2 %>% 
  mutate( pred.centered = fit.ns.AvePM..df...2.,
          se = se.fit.ns.AvePM..df...2.)

# 6c.iv Compute 95% confidence intervals 

predBMI.ns.2 <- predBMI.ns.2 %>% 
  mutate( lci.centered = pred.centered - 1.96*se,
          uci.centered = pred.centered + 1.96*se)

# 6c.v Keep only variables we need

predBMI.ns.2 <- predBMI.ns.2 %>% 
  dplyr::select(pred.centered, se, lci.centered, uci.centered)

# 6c.vi Combine with data 

predBMI.ns.2 <- predBMI.ns.2 %>% bind_cols(df)

# 6c.vii Uncenter 

predBMI.ns.2 <- predBMI.ns.2 %>% mutate(predBMI = pred.centered + mean(AveBMI),
                                        lciBMI = lci.centered + mean(AveBMI),
                                        uciBMI = uci.centered + mean(AveBMI))
# 6c.viii Plot
ggplot(predBMI.ns.2, aes(AvePM)) + 
  geom_line(aes(y = predBMI)) + 
  geom_line(aes(y = lciBMI), color = "darkgrey") + 
  geom_line(aes(y = uciBMI), color = "darkgrey") + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI (95% CI)") + 
  ylim(26, 29) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

# 6d Compare models with different degrees of freedom 

# 6d.i Construct models 

mod.ns.2 <- lm(AveBMI ~ ns(AvePM, df = 2) + FemaleUnemp + MaleUnemp + LTHS + 
                 MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm + ClimateRegion, data = df)

mod.ns.4 <- lm(AveBMI ~ ns(AvePM, df = 4) + FemaleUnemp + MaleUnemp + LTHS + 
                 MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm + ClimateRegion, data = df)

# 6d.ii Extract AIC of each model 

aic.mod.ns.2 <- AIC(mod.ns.2)
aic.mod.ns.3 <- AIC(mod.ns.3)
aic.mod.ns.4 <- AIC(mod.ns.4)

# 6d.iii Put AIC's into a table 

models_aic        <- data.frame( c("2 df", "3 df", "4 df"),
                                 c(aic.mod.ns.2, aic.mod.ns.3, aic.mod.ns.4))
names(models_aic) <- c("ModelName", "AIC")
models_aic

ggplot(predBMI.ns.3, aes(AvePM)) + 
  geom_line(aes(y = predBMI)) + 
  geom_line(aes(y = lciBMI), color = "darkgrey") + 
  geom_line(aes(y = uciBMI), color = "darkgrey") + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI (95% CI)") + 
  ylim(26, 29) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))


# Review pdf of plot: three_ns_model_plot.pdf

##** Class Question 2 **##
# Based on the AIC, which model would we prefer? 
# Looking at the plots, which model looks more plausible? 
##*****************************************************************## 

# 6d.iv dplyr::select the model with the lowest AIC
# here, which() identifies rows that meet the criteria 
# and the criteria is having 
# AIC equal to the minimum AIC 
# among the models in the table 

models_aic$ModelName[which(models_aic$AIC==min(models_aic$AIC))]

# 6e Create natural spline with gam()
# We can also use the gam() command to create the same model

mod.ns.gam.3 <- gam(AveBMI ~ ns(AvePM, df = 2) + FemaleUnemp + MaleUnemp + 
                      LTHS + MedHInc + MedHVal + PerBlack + PerLatinx + 
                      PerAsianAm + ClimateRegion, data = df)

# 6f Plot multiple models 
# 6f.i Create a column with the model name 
predBMI.ns.3  <- predBMI.ns.3  %>% 
  mutate(ModelName = "Natural Spline 3 df")
# 6f.ii Combine predictions from both models 
predBMI.tot <- bind_rows(predBMI.tot,
                         predBMI.ns.3)
# 6f.iii Order the variables as factors 
# This makes the legend easier to follow
predBMI.tot <- predBMI.tot %>% 
  mutate(ModelName = factor(ModelName, 
                            levels = c("Linear Term", "Quadratic Term", "Piecewise Linear Spline", 
                                       "Natural Spline 3 df", "Penalized Spline")))
# 6f.iv Plot 
all.models.plot <-  ggplot(predBMI.tot, aes(AvePM)) + 
  geom_line(aes(y = predBMI, color = ModelName)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI") + 
  ylim(26.5, 28.5) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

all.models.plot 

####*******************************
#### 7: Penalized Spline Term  ####
####*******************************

# 7a Construct model
# we can constuct penalized splines within gam() 
# default is 10 knots 
# only increase if need >~8.5 edf (with 10 knots our maximum edf is ~8.5)
# (edf = estimated degrees of freedom, vs. user-defined)
# penalty is estimated -- the model dplyr::selects the penalty 
# that leads to the highest gcv

mod.ps <- gam(AveBMI ~ s(AvePM) + FemaleUnemp + MaleUnemp + LTHS + 
                MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm +
                ClimateRegion, data = df)

# s(AvePM, fx = TRUE, k = df+1, bs = "cr") #EQUIVALENT TO NATURAL SPLINE NS FUNCTION

# 7b Model Summary 

summary(mod.ps)

# 7c Extract Penalty 
# this is the penalty estimated by the model 

mod.ps$sp

# 7d Model Plot 
# plot.gam offers a nice default plot
# a quick way to plot the change in predicted AveBMI with AvePM

plot(mod.ps)

# 7e Plot Exposure-Response Relationship with ggplot 
# we can also recreate this plot with ggplot

# 7e.i Construct predictions based on the model

predBMI.ps <- predict(mod.ps, se.fit = TRUE, type = "terms" )

# 7e.ii Convert to dataframe 

predBMI.ps <- as.data.frame(predBMI.ps)

# 7e.iii Combine predictions and standard errors
# it just has different names since its one term 

predBMI.ps <- predBMI.ps %>% 
  mutate( pred.centered = fit.s.AvePM.,
          se = se.fit.s.AvePM.)

# 7e.iv Compute 95% confidence intervals 

predBMI.ps <- predBMI.ps %>% 
  mutate( lci.centered = pred.centered - 1.96*se,
          uci.centered = pred.centered + 1.96*se)

# 7e.v Keep only variables we need 

predBMI.ps <- predBMI.ps %>% 
  dplyr::select(pred.centered, se, lci.centered, uci.centered)

# 7e.vi Combine with data 

predBMI.ps <- predBMI.ps %>% bind_cols(df)

# 7e.vii Uncenter data 

predBMI.ps <- predBMI.ps %>% mutate(predBMI = pred.centered + mean(AveBMI),
                                    lciBMI = lci.centered + mean(AveBMI),
                                    uciBMI = uci.centered + mean(AveBMI))
# 7e.viii Plot

ggplot(predBMI.ps, aes(AvePM)) + 
  geom_line(aes(y = predBMI)) + 
  geom_line(aes(y = lciBMI), color = "darkgrey") + 
  geom_line(aes(y = uciBMI), color = "darkgrey") + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI (95% CI)") + 
  ylim(26, 29) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

# 7f Plot multiple models 
# 7f.i Create a column with the model name 
predBMI.ps   <- predBMI.ps %>% mutate(ModelName = "Penalized Spline")
# 7f.ii Combine predictions from both models 
predBMI.tot <- bind_rows(predBMI.tot,
                         predBMI.ps)
# 7f.iii Order the variables as factors 
# This makes the legend easier to follow
predBMI.tot <- predBMI.tot %>% 
  mutate(ModelName = factor(ModelName, 
                            levels = c("Linear Term", "Quadratic Term", "Piecewise Linear Spline", 
                                       "Natural Spline 3 df", "Penalized Spline")))
# 7f.iv Plot 
all.models.plot <-  ggplot(predBMI.tot, aes(AvePM)) + 
  geom_line(aes(y = predBMI, color = ModelName)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI") + 
  ylim(26.5, 28.5) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))
all.models.plot 

##** Class Question 3 **##
# Do the models yield similar exposure-response curves?
# Which model would you report, and what information would help you choose? 
##*****************************************************************## 

####****************************
#### 8: Breakout Exercises #####
####****************************

# A) What is the association between Median Household Value and average BMI? 
#       What term best describes the relationship? 
# Test a linear model and 2 or 3 nonlinear terms
#(quadratic, piecewise linear spline, natural spline, penalized spline)
# (or more if you have time)

# B) Plot the exposure-response relationship of the final model you dplyr::selected.

####*************************************************
#### Footnote: Plotting All the Models Together #####
####*************************************************
# create a column of TermName for each dataframe of predictions 
# we will use this column to keep track of which model 
# the predictions came from 

# A Create linear model 
predBMI.lin <- predict(mod.lin, se.fit = TRUE, type = "terms" )
predBMI.lin <- as.data.frame(predBMI.lin)
predBMI.lin <- predBMI.lin %>% mutate(pred = fit.AvePM)
predBMI.lin <- predBMI.lin %>% dplyr::select(pred)
predBMI.lin <- predBMI.lin %>% bind_cols(df)
predBMI.lin <- predBMI.lin %>% mutate(predBMI = pred + mean(AveBMI))

# B Create a column with the model name 
# we will use this to keep track of the models once we combine them
predBMI.lin  <- predBMI.lin    %>% mutate(ModelName = "Linear")
predBMI.quad <- predBMI.quad   %>% mutate(ModelName = "Quadratic Term")
predBMI.pls  <- predBMI.pls    %>% mutate(ModelName = "Piecewise Linear Spline")
predBMI.ns.3  <- predBMI.ns.3  %>% mutate(ModelName = "Natural Spline 3 df")
predBMI.ps   <- predBMI.ps     %>% mutate(ModelName = "Penalized Spline")

# C Combine the predictions 

predBMI.tot <- bind_rows(predBMI.lin,
                         predBMI.quad, 
                         predBMI.pls,
                         predBMI.ns.3,
                         predBMI.ps)


predBMI.tot <- predBMI.tot %>% 
  mutate(ModelName = factor(ModelName, 
                         levels = c("Linear", "Quadratic Term", 
                                    "Piecewise Linear Spline",
                                    "Natural Spline 3 df", "Penalized Spline")))
# D Plot!

all.models.plot <-  ggplot(predBMI.tot, aes(AvePM)) + 
  geom_line(aes(y = predBMI, color = ModelName)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI") + 
  ylim(26, 28.5) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

all.models.plot 

# save plot as a pdf
pdf(paste0(ProjectFolder, "all_models_plot.pdf"), width = 10)
all.models.plot
dev.off()

####***************************
#### Footnote: Cubic Term #####
####***************************

# A Create model 
# we can create new terms within the model statement 
# using the I() command 

mod.cub <- lm(AveBMI ~ AvePM + I(AvePM^2) + I(AvePM^3) + 
                + FemaleUnemp + MaleUnemp + LTHS + 
                MedHInc + MedHVal + PerBlack + PerLatinx + PerAsianAm + ClimateRegion, 
              data = df)

# B Model Summary 

summary(mod.cub)

# C Construct predictions based on the model

predBMI.cub <- predict(mod.cub, se.fit = TRUE, type = "terms" )

# D convert to dataframe 

predBMI.cub <- as.data.frame(predBMI.cub)

# E Combine predictions and standard errors

predBMI.cub <- predBMI.cub %>% 
  mutate( pred = fit.AvePM + fit.I.AvePM.2.+ fit.I.AvePM.3.,
          se = se.fit.AvePM + se.fit.I.AvePM.2.+ se.fit.I.AvePM.3.)

# F Compute 95% confidence intervals 

predBMI.cub <- predBMI.cub %>% 
  mutate( lci = pred - 1.96*se,
          uci = pred + 1.96*se)

# G Keep only variables we need 

predBMI.cub <- predBMI.cub %>% dplyr::select(pred, se, lci, uci)

# H Combine with data 

predBMI.cub <- predBMI.cub %>% bind_cols(df)

# I Plot

ggplot(predBMI.cub, aes(AvePM)) + 
  geom_line(aes(y = pred)) + 
  geom_line(aes(y = lci), color = "darkgrey") + 
  geom_line(aes(y = uci), color = "darkgrey") + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted AveBMI") + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

####*******************************
#### Footnote: Multiple Plots #####
####*******************************

# A Create predictions and 95% CI for 2 df
# A1 Create predictions for 5 degrees of freedom
predBMI <- predict(mod.ns.2, se.fit = TRUE, type = "terms" )
# A2 Convert to dataframe 
predBMI <- as.data.frame(predBMI)
# A3 Combine predictions and standard errors
predBMI <- predBMI %>% 
  mutate( pred = fit.ns.AvePM..df...2.,
          se = se.fit.ns.AvePM..df...2.)
# A4 Compute 95% confidence intervals 
predBMI <- predBMI %>% 
  mutate( lci = pred - 1.96*se,
          uci = pred + 1.96*se)
# A5 Keep only variables we need 
predBMI2 <- predBMI %>% dplyr::select(pred, se, lci, uci) %>% 
  mutate(Model = "2 df")
# A6 Combine with data 
predBMI.2 <- predBMI2 %>% bind_cols(df)

# B1 Create predictions for 3 degrees of freedom
predBMI <- predict(mod.ns.3, se.fit = TRUE, type = "terms" )
# B2 convert to dataframe 
predBMI <- as.data.frame(predBMI)
# B3 Combine predictions and standard errors
predBMI <- predBMI %>% 
  mutate( pred = fit.ns.AvePM..df...3.,
          se = se.fit.ns.AvePM..df...3.)
# B4 Compute 95% confidence intervals 
predBMI <- predBMI %>% 
  mutate( lci = pred - 1.96*se,
          uci = pred + 1.96*se)
# B5 Keep only variables we need
predBMI2 <- predBMI %>% dplyr::select(pred, se, lci, uci)%>% 
  mutate(Model = "3 df")
# B6 Combine with data 
predBMI.3 <- predBMI2 %>% bind_cols(df)

# C Create predictions and 95% CI for 4 df
# C1 Create predictions for 4 degrees of freedom
predBMI <- predict(mod.ns.4, se.fit = TRUE, type = "terms" )
# C2 Convert to dataframe 
predBMI <- as.data.frame(predBMI)
# C3 Combine predictions and standard errors
predBMI <- predBMI %>% 
  mutate( pred = fit.ns.AvePM..df...4.,
          se = se.fit.ns.AvePM..df...4.)
# C4 Compute 95% confidence intervals 
predBMI <- predBMI %>% 
  mutate( lci = pred - 1.96*se,
          uci = pred + 1.96*se)
# C5 Keep only variables we need
predBMI2 <- predBMI %>% dplyr::select(pred, se, lci, uci) %>% 
  mutate(Model = "4 df")
# C6 Combine with data 
predBMI.4 <- predBMI2 %>% bind_cols(df)
# C7 combine all the data 
allModels <- bind_rows(predBMI.2, predBMI.3, predBMI.4)

# D Uncenter the data - add mean AveBMI
allModels <- allModels %>% mutate(predBMI = pred + mean(AveBMI),
                                  lciBMI = lci + mean(AveBMI),
                                  uciBMI = uci + mean(AveBMI))

# E Plot 

pdf(paste0(ProjectFolder, "three_ns_model_plot.pdf"))

# Without confidence intervals
ggplot(allModels, aes(AvePM)) + 
  geom_line(aes(y = predBMI, color = Model)) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI") + 
  ylim(26, 29) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

# With confidence intervals
ggplot(allModels, aes(AvePM)) + 
  geom_line(aes(y = predBMI, color = Model)) + 
  geom_line(aes(y = lciBMI, color = Model), alpha = 0.2) + 
  geom_line(aes(y = uciBMI, color = Model), alpha = 0.2) + 
  xlab(expression("Average Annual PM"[2.5])) + 
  ylab("Predicted BMI (95% CI)") + 
  ylim(26, 29) + 
  theme(axis.text = element_text(size = 15)) + 
  theme(axis.title = element_text(size = 20))

dev.off()


