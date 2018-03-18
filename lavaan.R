#Loading packages
install.packages("lavaan.shiny")
library(shiny)
library(lavaan.shiny)
lavaan.shiny()

#Loading the data
inov <- read.csv("~/Documents/GitHub/lavaan_shiny/inov.csv")
head(inov)

#CFA

CFA.model <- "excitement =~ Excitement1 + Excitement2 + Excitement3 + Excitement4
image =~ Image1 + Image2 + Image3 + Image4 + Image5
price =~ Price1 + Price2 + Price3
loyalty =~ Loyalty1 + Loyalty2 + Loyalty3 + Loyalty4
word =~ Word1 + Word2 + Word3 + Word4 
tolerance =~ Tolerance1 + Tolerance2 + Tolerance3 + Tolerance4 + Tolerance5
inovation =~ Inovação1 + Inovação2 + Inovação3 + Inovação4 + Inovação5 + Inovação6 "

fit.cfa <- cfa(CFA.model, data=inov) 
summary(fit.cfa, fit.measures=TRUE)

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

inovation ~ excitement + image + price + loyalty + word
"

fit.sem <- sem(SEM.model, data=inov)
summary(fit.sem, standardized=TRUE)
