library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

da1959hst<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1959_highest.csv")
save(da1959hst, file = "rawdata59hst.RData")
load("rawdata59hst.RData")


sov_model_2_1959_highst_base <- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                   data = da1959hst)
summary(sov_model_2_1959_highst_base)

sov_model_2_1959_highst <- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970)+oblast_center+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1959hst)
summary(sov_model_2_1959_highst)

da1959hst %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1970)-log(pop1959), x = log(mp1970)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1970), y = log(pop1970)-log(pop1959)),method = "lm" ,size = 1)

confint(sov_model_2_1959_highst_base)
confint(sov_model_2_1959_highst)

##################################################################################################################

da1959m<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1959_middle.csv")
save(da1959m, file = "rawdata59m.RData")
load("rawdata59m.RData")

sov_model_2_1959_middle_base<- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970),#+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                  data = da1959m)
summary(sov_model_2_1959_middle_base)


sov_model_2_1959_middle <- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970)+CityStatus+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                              data = da1959m)
summary(sov_model_2_1959_middle)

da1959m %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1970)-log(pop1959), x = log(mp1970)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1970), y = log(pop1970)-log(pop1959)),method = "lm" ,size = 1)

confint(sov_model_2_1959_middle_base)
confint(sov_model_2_1959_middle)

##################################################################################################################

da1959h<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1959_high.csv")
save(da1959h, file = "rawdata59h.RData")
load("rawdata59h.RData")


sov_model_2_1959_high_base <- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                 data = da1959h)
summary(sov_model_2_1959_high_base)

sov_model_2_1959_high <- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970)+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                            data = da1959h)
summary(sov_model_2_1959_high)

da1959h %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1970)-log(pop1959), x = log(mp1970)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1970), y = log(pop1970)-log(pop1959)),method = "lm" ,size = 1)

confint(sov_model_2_1959_high_base)
confint(sov_model_2_1959_high)

###################################################################################################################

da1959l<- rio::import("C:/Users/Yana/Desktop/git/machine_learning/population/py/df_1959_low.csv")
save(da1959l, file = "rawdata59l.RData")
load("rawdata59l.RData")


sov_model_2_1959_low_base <- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970),#+oblast_center+CityStatus+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                                data = da1959l)
summary(sov_model_2_1959_low_base)

sov_model_2_1959_low <- lm(formula = log(pop1970)-log(pop1959)~ log(mp1970)+oblast_center+RaionCenter+URAL+YUZHNY+DALNEV+PRIVOL+SEVER_ZAPAD+SIBIRSKII+HBR+LBR,#+smp+spop,
                           data = da1959l)
summary(sov_model_2_1959_low)

da1959l %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = log(pop1970)-log(pop1959), x = log(mp1970)), alpha =0.25)+
  geom_smooth(aes(x = log(mp1970), y = log(pop1970)-log(pop1959)),method = "lm" ,size = 1)

confint(sov_model_2_1959_low_base)
confint(sov_model_2_1959_low)
