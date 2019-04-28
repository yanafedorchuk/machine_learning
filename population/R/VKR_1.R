# dt <- rio::import("C:/Users/Yana/Downloads/f_pls.xlsx")
# save(dt, file = "rawdata1.RData")
# load("rawdata1.RData")
library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)

data2lm <-dt %>% select(-c(1,3:5,9:14))%>%
  melt(id.vars = c("ID89", "oblast_center", "CityStatus", "RaionCenter")) %>%
  mutate(variable = as.character(variable),
         ratio = ifelse(substring(variable, 1, 3) == "pop", "pop", 
                        ifelse(substring(variable, 1, 4) == "spop", "spop",
                               ifelse(substring(variable, 1, 3) == "smp", "smp","mp"))),
         year = substring(variable, nchar(variable)-3, nchar(variable))) %>%
  select(-variable) %>%
  dcast(ID89+oblast_center+CityStatus+ RaionCenter+year~ratio, value.var = "value") %>%
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
           soviet_true=Soviet*log_mp) %>%
  ungroup() 
summary(data2lm)
set.seed(2019)
data2lm %>%
  na.omit() %>%
  #sample_n(1000) %>%
  ggplot() +geom_point(aes(y = pop_log-pop_log_lag, x = log_mp, color = pop_log), alpha =1)+
  scale_color_gradient2(midpoint=9, low="blue", mid="white",
                        high="darkred", space ="Lab" )+
  facet_wrap(vars(year),  drop = TRUE, scales = "free")+
  geom_smooth(aes(x = log_mp, y = pop_log-pop_log_lag), method = "lm",size = 1)
model <- lm(formula = pop~ pop_lag+mp+oblast_center+CityStatus+RaionCenter+year, data = data2lm)
log_model <- lm(formula = log(pop)~ pop_lag+mp+oblast_center+CityStatus+RaionCenter+year, data = data2lm)
summary(log_model)
# добавим дамми перемнную для ссср
sov_model <- lm(formula = pop_log-pop_log_lag~ 0+log_mp+mp+oblast_center+CityStatus+RaionCenter+year+soviet_true,#+smp+spop,
                data = data2lm)
summary(log_model)
summary(sov_model)
# cairo_pdf для русских подписей к графикам
# p <- ggplot()+...
#cairo_pdf(file = "plot.pdf", width =, heigth =)
#print(p)
#dev.off()
#