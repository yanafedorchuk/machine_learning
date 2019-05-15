library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da1970hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1970_highest.csv")
save(da1970hst, file = "rawdata70hst.RData")
load("rawdata70hst.RData")


sov_model_2_1970_highst_base <- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da1970hst)
summary(sov_model_2_1970_highst_base)

sov_model_2_1970_highst <- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1970hst)
summary(sov_model_2_1970_highst)

da1970hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1979)-log(pop1970), x = log(mp1979)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1979), y = log(pop1979)-log(pop1970)),method = "lm" ,size = 1)

confint(sov_model_2_1970_highst_base)
confint(sov_model_2_1970_highst)

##################################################################################################################

da1970m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1970_middle.csv")
save(da1970m, file = "rawdata70m.RData")
load("rawdata70m.RData")

sov_model_2_1970_middle_base<- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                  data = da1970m)
summary(sov_model_2_1970_middle_base)


sov_model_2_1970_middle <- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1970m)
summary(sov_model_2_1970_middle)

da1970m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1979)-log(pop1970), x = log(mp1979)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1979), y = log(pop1979)-log(pop1970)),method = "lm" ,size = 1)

confint(sov_model_2_1970_middle_base)
confint(sov_model_2_1970_middle)

##################################################################################################################

da1970h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1970_high.csv")
save(da1970h, file = "rawdata70h.RData")
load("rawdata70h.RData")


sov_model_2_1970_high_base <- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da1970h)
summary(sov_model_2_1970_high_base)

sov_model_2_1970_high <- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da1970h)
summary(sov_model_2_1970_high)

da1970h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1979)-log(pop1970), x = log(mp1979)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1979), y = log(pop1979)-log(pop1970)),method = "lm" ,size = 1)

confint(sov_model_2_1970_high_base)
confint(sov_model_2_1970_high)

###################################################################################################################

da1970l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1970_low.csv")
save(da1970l, file = "rawdata70l.RData")
load("rawdata70l.RData")


sov_model_2_1970_low_base <- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                data = da1970l)
summary(sov_model_2_1970_low_base)

sov_model_2_1970_low <- lm(formula = log(pop1979)-log(pop1970)~ log(mp1979)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                           data = da1970l)
summary(sov_model_2_1970_low)

da1970l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1979)-log(pop1970), x = log(mp1979)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1979), y = log(pop1979)-log(pop1970)),method = "lm" ,size = 1)

confint(sov_model_2_1970_low_base)
confint(sov_model_2_1970_low)
