library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da2002hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_2002_highest.csv")
save(da2002hst, file = "rawdata02hst.RData")
load("rawdata02hst.RData")


sov_model_2_2002_highst_base <- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da2002hst)
summary(sov_model_2_2002_highst_base)

sov_model_2_2002_highst <- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da2002hst)
summary(sov_model_2_2002_highst)

da2002hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2010)-log(pop2002), x = log(mp2010)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2010), y = log(pop2010)-log(pop2002)),method = "lm" ,size = 1)

confint(sov_model_2_2002_highst_base)
confint(sov_model_2_2002_highst)

##################################################################################################################

da2002m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_2002_middle.csv")
save(da2002m, file = "rawdata02m.RData")
load("rawdata02m.RData")

sov_model_2_2002_middle_base<- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                  data = da2002m)
summary(sov_model_2_2002_middle_base)


sov_model_2_2002_middle <- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010)+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da2002m)
summary(sov_model_2_2002_middle)

da2002m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2010)-log(pop2002), x = log(mp2010)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2010), y = log(pop2010)-log(pop2002)),method = "lm" ,size = 1)

confint(sov_model_2_2002_middle_base)
confint(sov_model_2_2002_middle)

##################################################################################################################

da2002h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_2002_high.csv")
save(da2002h, file = "rawdata02h.RData")
load("rawdata02h.RData")


sov_model_2_2002_high_base <- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da2002h)
summary(sov_model_2_2002_high_base)

sov_model_2_2002_high <- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da2002h)
summary(sov_model_2_2002_high)

da2002h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2010)-log(pop2002), x = log(mp2010)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2010), y = log(pop2010)-log(pop2002)),method = "lm" ,size = 1)

confint(sov_model_2_2002_high_base)
confint(sov_model_2_2002_high)

###################################################################################################################

da2002l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_2002_low.csv")
save(da2002l, file = "rawdata02l.RData")
load("rawdata02l.RData")


sov_model_2_2002_low_base <- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                data = da2002l)
summary(sov_model_2_2002_low_base)

sov_model_2_2002_low <- lm(formula = log(pop2010)-log(pop2002)~ log(mp2010)+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                           data = da2002l)
summary(sov_model_2_2002_low)

da2002l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2010)-log(pop2002), x = log(mp2010)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2010), y = log(pop2010)-log(pop2002)),method = "lm" ,size = 1)

confint(sov_model_2_2002_low_base)
confint(sov_model_2_2002_low)
