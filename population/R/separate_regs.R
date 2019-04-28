library(lmtest)
library(dplyr)
library(broom)
library(ggplot2)
library(forecast)
library(strucchange)
library(tseries)
library(readxl)



my_df <- read.csv(file="C:/Users/Yana/Downloads/f_cut - f.csv", header=TRUE, sep=",")
model<-lm(formula=log_pop2010-log_pop2002 ~ log_mp2010, data = my_df)
summary(model)
