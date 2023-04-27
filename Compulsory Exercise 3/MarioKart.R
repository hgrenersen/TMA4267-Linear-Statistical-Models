#setwd("C:/Users/eirik/OneDrive - NTNU/3. klasse/V2023/Linstat")
library(FrF2)
library(ggplot2)
library(ggfortify)
library(nortest)
library(MASS)

#loading the data
MarioKart.df <- read.table("MarioKart.csv", sep=";", header=T, dec = ",")
colnames(MarioKart.df)[1] = "Character" #changing the first colname to 'Character'

#Adding factors and response to a new dataframe
Character <- as.factor(ifelse(MarioKart.df$Character == MarioKart.df$Character[1] , 1, -1))
Body <- as.factor(ifelse(MarioKart.df$Body == MarioKart.df$Body[1] , 1, -1))
Tire <- as.factor(ifelse(MarioKart.df$Tire == MarioKart.df$Tire[1] , 1, -1))
Glider <- as.factor(ifelse(MarioKart.df$Glider == MarioKart.df$Glider[1] , 1, -1))
Time <- MarioKart.df$Average
Experiment.df <- data.frame(Character, Body, Tire, Glider, Time)

#creating linear model
Experiment.lm <- lm(Time ~ (.)^2, data = Experiment.df)
summary(Experiment.lm)

#ANOVA
anova(Experiment.lm)

#Residual plot
autoplot(Experiment.lm, smooth.colour = NA)

#Standardized residuals
stdres <- stdres(Experiment.lm)
ad.test(stdres)

#Plots
MEPlot(Experiment.lm)
IAPlot(Experiment.lm)
