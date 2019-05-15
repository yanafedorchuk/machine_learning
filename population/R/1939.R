library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da1939hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1939_highest.csv")
save(da1939hst, file = "rawdata39hst.RData")
load("rawdata39hst.RData")


sov_model_2_1939_highst_base <- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da1939hst)
summary(sov_model_2_1939_highst_base)

sov_model_2_1939_highst <- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1939hst)
summary(sov_model_2_1939_highst)

da1939hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1959)-log(pop1939), x = log(mp1959)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1959), y = log(pop1959)-log(pop1939)),method = "lm" ,size = 1)

confint(sov_model_2_1939_highst_base)
confint(sov_model_2_1939_highst)

##################################################################################################################

da1939m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1939_middle.csv")
save(da1939m, file = "rawdata39m.RData")
load("rawdata39m.RData")

sov_model_2_1939_middle_base<- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                  data = da1939m)
summary(sov_model_2_1939_middle_base)


sov_model_2_1939_middle <- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1939m)
summary(sov_model_2_1939_middle)

da1939m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1959)-log(pop1939), x = log(mp1959)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1959), y = log(pop1959)-log(pop1939)),method = "lm" ,size = 1)

confint(sov_model_2_1939_middle_base)
confint(sov_model_2_1939_middle)

##################################################################################################################

da1939h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1939_high.csv")
save(da1939h, file = "rawdata39h.RData")
load("rawdata39h.RData")


sov_model_2_1939_high_base <- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da1939h)
summary(sov_model_2_1939_high_base)

sov_model_2_1939_high <- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da1939h)
summary(sov_model_2_1939_high)

da1939h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1959)-log(pop1939), x = log(mp1959)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1959), y = log(pop1959)-log(pop1939)),method = "lm" ,size = 1)

confint(sov_model_2_1939_high_base)
confint(sov_model_2_1939_high)

###################################################################################################################

da1939l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1939_low.csv")
save(da1939l, file = "rawdata39l.RData")
load("rawdata39l.RData")


sov_model_2_1939_low_base <- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                data = da1939l)
summary(sov_model_2_1939_low_base)

sov_model_2_1939_low <- lm(formula = log(pop1959)-log(pop1939)~ log(mp1959)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                           data = da1939l)
summary(sov_model_2_1939_low)

da1939l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1959)-log(pop1939), x = log(mp1959)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1959), y = log(pop1959)-log(pop1939)),method = "lm" ,size = 1)

confint(sov_model_2_1939_low_base)
confint(sov_model_2_1939_low)
