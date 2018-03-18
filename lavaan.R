#Loading packages
install.packages("lavaan.shiny")
library(shiny)
library(lavaan.shiny)
lavaan.shiny()


#Loading the data
inov <- read.csv("~/Documents/GitHub/lavaan_shiny/inov.csv")
inov_clean <- read.csv("~/Documents/GitHub/lavaan_shiny/inov_clean.csv", sep=";")


#We are going to do a previous pre processing. We are going to eliminate outliers (new ds: 93 obs)
library(ggplot2)
library(Hmisc)
str(inov)
hist.data.frame(inov)

  
#Clean version
str(inov_clean)
hist.data.frame(inov_clean[4:7])
hist.data.frame(inov_clean[8:12])
hist.data.frame(inov_clean[13:15])
hist.data.frame(inov_clean[16:19])
hist.data.frame(inov_clean[20:23])
hist.data.frame(inov_clean[24:28])
hist.data.frame(inov_clean[29:34])
hist.data.frame(inov_clean[35:39])
#CFA

CFA.model <- "excitement =~ Excitement1 + Excitement2 + Excitement3 + Excitement4
image =~ Image1 + Image2 + Image3 + Image4 + Image5
price =~ Price1 + Price2 + Price3
loyalty =~ Loyalty1 + Loyalty2 + Loyalty3 + Loyalty4
word =~ Word1 + Word2 + Word3 + Word4 
tolerance =~ Tolerance1 + Tolerance2 + Tolerance3 + Tolerance4 + Tolerance5
inovation =~ Inovação1 + Inovação2 + Inovação3 + Inovação4 + Inovação5 + Inovação6 "

fit.cfa <- cfa(CFA.model, data=inov_clean) 
summary(fit.cfa, fit.measures=TRUE, rsq=T)

##There's no significance in 3points: tolerance2 e tolerance4 and price1. Lets build a model without those

CFA.model2 <- "excitement =~ Excitement1 + Excitement2 + Excitement3 + Excitement4
image =~ Image1 + Image2 + Image3 + Image4 + Image5
price =~ Price2 + Price3
loyalty =~ Loyalty1 + Loyalty2 + Loyalty3 + Loyalty4
word =~ Word1 + Word2 + Word3 + Word4 
tolerance =~ Tolerance1 + Tolerance2 + Tolerance3 + Tolerance4 + Tolerance5
inovation =~ Inovação1 + Inovação2 + Inovação3 + Inovação4 + Inovação5 + Inovação6 "


fit.cfa2 <- cfa(CFA.model2, data=inov) 
summary(fit.cfa2, fit.measures=TRUE)


#SEM

SEM.model <- "excitement =~ Excitement1 + Excitement2 + Excitement3 + Excitement4
image =~ Image1 + Image2 + Image3 + Image4 + Image5
price =~ Price2 + Price3
loyalty =~ Loyalty1 + Loyalty2 + Loyalty3 + Loyalty4
word =~ Word1 + Word2 + Word3 + Word4 
tolerance =~ Tolerance1 + Tolerance3 + Tolerance5
inovation =~ Inovação1 + Inovação2 + Inovação3 + Inovação4 + Inovação5 + Inovação6 

excitement ~ inovation
price ~ inovation
loyalty ~ inovation
image ~ inovation
word ~ inovation
tolerance ~ inovation"


fit.sem <- sem(SEM.model, data=inov)
summary(fit.sem, standardized=TRUE, fit.measures=T)

##Testing this with the means, since we have multiple groups

fit.sem <- sem(SEM.model, data=inov, meanstructure=TRUE)
summary(fit.sem, standardized=TRUE)


#Fitting a model without price and loyalty
SEM.model2 <- "excitement =~ Excitement1 + Excitement2 + Excitement3 + Excitement4
image =~ Image1 + Image2 + Image3 + Image4 + Image5
word =~ Word1 + Word2 + Word3 + Word4 
tolerance =~ Tolerance1 + Tolerance3 + Tolerance5
inovation =~ Inovação1 + Inovação2 + Inovação3 + Inovação4 + Inovação5 + Inovação6 

excitement ~ inovation
image ~ inovation
word ~ inovation
tolerance ~ inovation"

fit.sem2 <- sem(SEM.model2, data=inov_clean)
summary(fit.sem2, standardized=TRUE, fit.measures=T, rsq=T)

