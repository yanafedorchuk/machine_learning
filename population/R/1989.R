library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da1989hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1989_highest.csv")
save(da1989hst, file = "rawdata89hst.RData")
load("rawdata89hst.RData")


sov_model_2_1989_highst_base <- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da1989hst)
summary(sov_model_2_1989_highst_base)

sov_model_2_1989_highst <- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1989hst)
summary(sov_model_2_1989_highst)

da1989hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2002)-log(pop1989), x = log(mp2002)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2002), y = log(pop2002)-log(pop1989)),method = "lm" ,size = 1)

confint(sov_model_2_1989_highst_base)
confint(sov_model_2_1989_highst)

##################################################################################################################

da1989m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1989_middle.csv")
save(da1989m, file = "rawdata89m.RData")
load("rawdata89m.RData")

sov_model_2_1989_middle_base<- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                  data = da1989m)
summary(sov_model_2_1989_middle_base)


sov_model_2_1989_middle <- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1989m)
summary(sov_model_2_1989_middle)

da1989m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2002)-log(pop1989), x = log(mp2002)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2002), y = log(pop2002)-log(pop1989)),method = "lm" ,size = 1)

confint(sov_model_2_1989_middle_base)
confint(sov_model_2_1989_middle)

##################################################################################################################

da1989h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1989_high.csv")
save(da1989h, file = "rawdata89h.RData")
load("rawdata89h.RData")


sov_model_2_1989_high_base <- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da1989h)
summary(sov_model_2_1989_high_base)

sov_model_2_1989_high <- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da1989h)
summary(sov_model_2_1989_high)

da1989h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2002)-log(pop1989), x = log(mp2002)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2002), y = log(pop2002)-log(pop1989)),method = "lm" ,size = 1)

confint(sov_model_2_1989_high_base)
confint(sov_model_2_1989_high)

###################################################################################################################

da1989l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1989_low.csv")
save(da1989l, file = "rawdata89l.RData")
load("rawdata89l.RData")


sov_model_2_1989_low_base <- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                data = da1989l)
summary(sov_model_2_1989_low_base)

sov_model_2_1989_low <- lm(formula = log(pop2002)-log(pop1989)~ log(mp2002)+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                           data = da1989l)
summary(sov_model_2_1989_low)

da1989l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop2002)-log(pop1989), x = log(mp2002)), alpha =0.25)+
  geom_smooth(aes(x = log(mp2002), y = log(pop2002)-log(pop1989)),method = "lm" ,size = 1)

confint(sov_model_2_1989_low_base)
confint(sov_model_2_1989_low)
