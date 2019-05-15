library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da1897hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1897_highest.csv")
save(da1897hst, file = "rawdata97hst.RData")
load("rawdata97hst.RData")


sov_model_2_1897_highst_base <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1897hst)
summary(sov_model_2_1897_highst_base)

sov_model_2_1897_highst <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1897hst)
summary(sov_model_2_1897_highst)

da1897hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1926)-log(pop1897), x = log(mp1926)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1926), y = log(pop1926)-log(pop1897)),method = "lm" ,size = 1)

confint(sov_model_2_1897_highst_base)
confint(sov_model_2_1897_highst)

##################################################################################################################

da1897m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1897_middle.csv")
save(da1897m, file = "rawdata97m.RData")
load("rawdata97m.RData")

sov_model_2_1897_middle_base<- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1897m)
summary(sov_model_2_1897_middle_base)


sov_model_2_1897_middle <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1897m)
summary(sov_model_2_1897_middle)

da1897m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1926)-log(pop1897), x = log(mp1926)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1926), y = log(pop1926)-log(pop1897)),method = "lm" ,size = 1)

confint(sov_model_2_1897_middle_base)
confint(sov_model_2_1897_middle)

##################################################################################################################

da1897h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1897_high.csv")
save(da1897h, file = "rawdata97h.RData")
load("rawdata97h.RData")


sov_model_2_1897_high_base <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da1897h)
summary(sov_model_2_1897_high_base)

sov_model_2_1897_high <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1897h)
summary(sov_model_2_1897_high)

da1897h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1926)-log(pop1897), x = log(mp1926)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1926), y = log(pop1926)-log(pop1897)),method = "lm" ,size = 1)

confint(sov_model_2_1897_high_base)
confint(sov_model_2_1897_high)

###################################################################################################################

da1897l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1897_low.csv")
save(da1897l, file = "rawdata97l.RData")
load("rawdata97l.RData")


sov_model_2_1897_low_base <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da1897l)
summary(sov_model_2_1897_low_base)

sov_model_2_1897_low <- lm(formula = log(pop1926)-log(pop1897)~ log(mp1926)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da1897l)
summary(sov_model_2_1897_low)

da1897l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1926)-log(pop1897), x = log(mp1926)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1926), y = log(pop1926)-log(pop1897)),method = "lm" ,size = 1)

confint(sov_model_2_1897_low_base)
confint(sov_model_2_1897_low)
