dat <- rio::import("C:/Users/Yana/Downloads/f_pls_pls (1).xlsx")
save(dat, file = "rawdata4.RData")
load("rawdata4.RData")
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

data3lm <-dat %>% select(-c(1,3:5,9:14))%>%
  melt(id.vars = c("ID89", "oblast_center", "CityStatus", "RaionCenter", "URAL", "YUZHNY" ,"DALNEV" , "PRIVOL", "SEVER_ZAPAD", "SIBIRSKII", "TSENTRALNY", "HBR", "LBR")) %>% #, "URAL", "YUZHNY" ,"DALNEV" , "PRIVOL", "SEVER_ZAPAD", "SIBIRSKII", "Tcentralny"
  mutate(variable = as.character(variable),
         ratio = ifelse(substring(variable, 1, 3) == "pop", "pop", 
                        ifelse(substring(variable, 1, 4) == "spop", "spop",
                               ifelse(substring(variable, 1, 3) == "smp", "smp","mp"))),
         year = substring(variable, nchar(variable)-3, nchar(variable))) %>%
  select(-variable) %>%
  dcast(ID89+oblast_center+CityStatus+ RaionCenter+year+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+TSENTRALNY+HBR+LBR~ratio, value.var = "value") %>%
  mutate(Soviet = ifelse(as.numeric(year)<=1991, TRUE, FALSE)) %>%
  mutate(year = as.factor(year),
         mp = as.numeric(mp),
         pop = as.numeric(pop),
         smp = as.numeric(smp),
         spop = as.numeric(spop)) %>%
  mutate(pop =ifelse(pop == 0, NA, pop)) %>%
  group_by(ID89) %>%
  mutate(pop_log_lag = lag(log(pop)),
         pop_log = log(pop),
         log_mp=log(mp),
         log_mp_lag=lag(log(mp)),
         soviet_true=Soviet*log_mp) %>%
  ungroup() 
summary(data3lm)
set.seed(2019)
data3lm %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = pop_log-pop_log_lag, x = log_mp, color = pop_log), alpha =1)+
  scale_color_gradient2(midpoint=9, low="blue", mid="white",
                        high="darkred", space ="Lab" )+
  facet_wrap(vars(year),  drop = TRUE, scales = "free")+
  geom_smooth(aes(x = log_mp, y = pop_log-pop_log_lag), method = "lm",size = 1)
sov_model_1 <- lm(formula = pop_log-pop_log_lag~ 0+log_mp+smp+oblast_center+CityStatus+RaionCenter+year+soviet_true+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+TSENTRALNY+HBR+LBR,#+smp+spop,
                data = data3lm)
summary(sov_model_1)

sov_model_2 <- lm(formula = pop_log-pop_log_lag~ 0+log_mp+smp+oblast_center+CityStatus+RaionCenter+year+soviet_true+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                  data = data3lm)
summary(sov_model_2)
