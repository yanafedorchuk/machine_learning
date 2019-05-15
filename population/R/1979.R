library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da1979hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1979_highest.csv")
save(da1979hst, file = "rawdata79hst.RData")
load("rawdata79hst.RData")


sov_model_2_1979_highst_base <- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da1979hst)
summary(sov_model_2_1979_highst_base)

sov_model_2_1979_highst <- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1979hst)
summary(sov_model_2_1979_highst)

da1979hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1989)-log(pop1979), x = log(mp1989)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1989), y = log(pop1989)-log(pop1979)),method = "lm" ,size = 1)

confint(sov_model_2_1979_highst_base)
confint(sov_model_2_1979_highst)

##################################################################################################################

da1979m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1979_middle.csv")
save(da1979m, file = "rawdata79m.RData")
load("rawdata79m.RData")

sov_model_2_1979_middle_base<- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                  data = da1979m)
summary(sov_model_2_1979_middle_base)


sov_model_2_1979_middle <- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1979m)
summary(sov_model_2_1979_middle)

da1979m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1989)-log(pop1979), x = log(mp1989)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1989), y = log(pop1989)-log(pop1979)),method = "lm" ,size = 1)

confint(sov_model_2_1979_middle_base)
confint(sov_model_2_1979_middle)

##################################################################################################################

da1979h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1979_high.csv")
save(da1979h, file = "rawdata79h.RData")
load("rawdata79h.RData")


sov_model_2_1979_high_base <- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da1979h)
summary(sov_model_2_1979_high_base)

sov_model_2_1979_high <- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da1979h)
summary(sov_model_2_1979_high)

da1979h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1989)-log(pop1979), x = log(mp1989)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1989), y = log(pop1989)-log(pop1979)),method = "lm" ,size = 1)

confint(sov_model_2_1979_high_base)
confint(sov_model_2_1979_high)

###################################################################################################################

da1979l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1979_low.csv")
save(da1979l, file = "rawdata79l.RData")
load("rawdata79l.RData")


sov_model_2_1979_low_base <- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                data = da1979l)
summary(sov_model_2_1979_low_base)

sov_model_2_1979_low <- lm(formula = log(pop1989)-log(pop1979)~ log(mp1989)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                           data = da1979l)
summary(sov_model_2_1979_low)

da1979l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1989)-log(pop1979), x = log(mp1989)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1989), y = log(pop1989)-log(pop1979)),method = "lm" ,size = 1)

confint(sov_model_2_1979_low_base)
confint(sov_model_2_1979_low)
