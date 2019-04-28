data <- rio::import("C:/Users/Yana/Downloads/mp_n_df (1).xlsx") %>% edit
#save(dt, file = "rawdata.RData")
#load("rawdata.RData")
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
data2lm <- data %>% select(-c(1,3:5,9:14))%>%
  melt(id.vars = c("ID89", "oblast_center", "CityStatus", "RaionCenter")) %>%
  mutate(variable = as.character(variable),
         ratio = ifelse(substring(variable, 1, 3) == "pop", "pop", "mp"),
         year = substring(variable, nchar(variable)-3, nchar(variable))) %>%
  select(-variable) %>%
  dcast(ID89+oblast_center+CityStatus+ RaionCenter+year~ratio, value.var = "value") %>%
  mutate(Soviet = ifelse(as.numeric(year)<=1991, TRUE, FALSE)) %>%
  mutate(year = as.factor(year),
         mp = as.numeric(mp),
         pop = as.numeric(pop)) %>%
  mutate(pop =ifelse(pop == 0, NA, pop)) %>%
  group_by(ID89) %>%
  mutate(pop_lag = lag(pop)) %>%
  ungroup() 
summary(data2lm)
set.seed(2019)
data2lm %>%
  sample_n(1000) %>%
  ggplot() +geom_smooth(aes(y = log(pop), x = mp))
model <- lm(formula = pop~ pop_lag+mp+oblast_center+CityStatus+RaionCenter+year, data = data2lm)
log_model <- lm(formula = log(pop)~ pop_lag+mp+oblast_center+CityStatus+RaionCenter+year, data = data2lm)
summary(log_model)
# добавим дамми перемнную для ссср
sov_model <- lm(formula = log(pop)~ 0+pop_lag+mp+oblast_center+CityStatus+RaionCenter+year+Soviet*mp, data = data2lm)
summary(log_model)
summary(sov_model)
