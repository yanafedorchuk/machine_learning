library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da1926hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1926_highest.csv")
save(da1926hst, file = "rawdata26hst.RData")
load("rawdata26hst.RData")


sov_model_2_1926_highst_base <- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da1926hst)
summary(sov_model_2_1926_highst_base)

sov_model_2_1926_highst <- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1926hst)
summary(sov_model_2_1926_highst)

da1926hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1939)-log(pop1926), x = log(mp1939)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1939), y = log(pop1939)-log(pop1926)),method = "lm" ,size = 1)

confint(sov_model_2_1926_highst_base)
confint(sov_model_2_1926_highst)

##################################################################################################################

da1926m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1926_middle.csv")
save(da1926m, file = "rawdata26m.RData")
load("rawdata26m.RData")

sov_model_2_1926_middle_base<- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                  data = da1926m)
summary(sov_model_2_1926_middle_base)


sov_model_2_1926_middle <- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1926m)
summary(sov_model_2_1926_middle)

da1926m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1939)-log(pop1926), x = log(mp1939)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1939), y = log(pop1939)-log(pop1926)),method = "lm" ,size = 1)

confint(sov_model_2_1926_middle_base)
confint(sov_model_2_1926_middle)

##################################################################################################################

da1926h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1926_high.csv")
save(da1926h, file = "rawdata26h.RData")
load("rawdata26h.RData")


sov_model_2_1926_high_base <- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da1926h)
summary(sov_model_2_1926_high_base)

sov_model_2_1926_high <- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da1926h)
summary(sov_model_2_1926_high)

da1926h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1939)-log(pop1926), x = log(mp1939)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1939), y = log(pop1939)-log(pop1926)),method = "lm" ,size = 1)

confint(sov_model_2_1926_high_base)
confint(sov_model_2_1926_high)

###################################################################################################################

da1926l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1926_low.csv")
save(da1926l, file = "rawdata26l.RData")
load("rawdata26l.RData")


sov_model_2_1926_low_base <- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                data = da1926l)
summary(sov_model_2_1926_low_base)

sov_model_2_1926_low <- lm(formula = log(pop1939)-log(pop1926)~ log(mp1939)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                           data = da1926l)
summary(sov_model_2_1926_low)

da1926l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1939)-log(pop1926), x = log(mp1939)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1939), y = log(pop1939)-log(pop1926)),method = "lm" ,size = 1)

confint(sov_model_2_1926_low_base)
confint(sov_model_2_1926_low)
