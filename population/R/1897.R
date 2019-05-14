da1897 <- rio::import("C:/Users/Yana/Downloads/1897_low.xlsx")
save(da1897, file = "rawdata97.RData")
load("rawdata97.RData")
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

data1897lm <-da1897 %>% select(-c(1,3:5,9:14))%>%
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
summary(data1897lm)

da1897h <- rio::import("C:/Users/Yana/Downloads/1897_high.xlsx")
save(da1897h, file = "rawdata97h.RData")
load("rawdata97h.RData")
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

data1897hlm <-da1897h %>% select(-c(1,3:5,9:14))%>%
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
summary(data1897hlm)

set.seed(2019)
data1897lm %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = pop_log-pop_log_lag, x = log_mp, color = pop_log), alpha =1)+
  scale_color_gradient2(midpoint=9, low="blue", mid="white",
                        high="darkred", space ="Lab" )+
  facet_wrap(vars(year),  drop = TRUE, scales = "free")+
  geom_smooth(aes(x = log_mp, y = pop_log-pop_log_lag),size = 1)
sov_model_1 <- lm(formula = pop_log-pop_log_lag~ log_mp+smp+CityStatus+oblast_center+RaionCenter+year+soviet_true+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+TSENTRALNY+HBR+LBR,#+smp+spop,
                  data = data1897lm)
summary(sov_model_1)

sov_model_2_1897 <- lm(formula = pop_log-pop_log_lag~ log_mp+smp+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                  data = data1897lm)
summary(sov_model_2_1897)

sov_model_2_1897_high <- lm(formula = pop_log-pop_log_lag~ log_mp+smp+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                       data = data1897hlm)
summary(sov_model_2_1897_high)

data3lm %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = pop_log-pop_log_lag, x = log_mp, color = pop_log), alpha =0.25)+
  geom_smooth(aes(x = log_mp, y = pop_log-pop_log_lag) ,size = 1)

da1926hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1926_highest.csv")
save(da1926hst, file = "rawdata26hst.RData")
load("rawdata26hst.RData")
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

data1926hstlm <-da1926hst %>% select(-c(1,3:5,9:14))%>%
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
summary(data1926hstlm)

sov_model_2_1926_highst <- lm(formula = pop_log-pop_log_lag~ log_mp+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = data1926hstlm)
summary(sov_model_2_1926_highst)


da1897hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1897_highest.csv")
save(da1897hst, file = "rawdata97hst.RData")
load("rawdata97hst.RData")


sov_model_2_1897_highst <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1897hst)
summary(sov_model_2_1897_highst)
library(qplot)
qplot(y = log(pop1926)-log(pop1897), x = log(mp1926), data=da1897hst)

da1897hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1926)-log(pop1897), x = log(mp1926)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1926), y = log(pop1926)-log(pop1897)),method = "lm" ,size = 1)

da1897m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1897_middle.csv")
save(da1897m, file = "rawdata97m.RData")
load("rawdata97m.RData")


sov_model_2_1897_middle <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1897m)
summary(sov_model_2_1897_middle)

da1897m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1926)-log(pop1897), x = log(mp1926)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1926), y = log(pop1926)-log(pop1897)),method = "lm" ,size = 1)
