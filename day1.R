install.packages("tidyverse")

library(tidyverse)
NTL_1993 <- read_csv("Data/Extensionzonalstat_101993.csv")
GDP_raw <- readxl::read_xlsx("Data/WDI_2014_11.xlsx", sheet = 6)
GDP_NY <- GDP_raw %>% 
  filter(`Indicator Code`== "NY.GDP.MKTP.KN")
Fiw <- read_csv("Data/FIW.csv")

Estimations_data <- read_dta("Data/Estimations.dta")
library(haven)
??view_df
install.packages("sjPlot")
library(sjPlot)
view_df(Estimations_data)

#使用変数名
#lndn13,fiw, lngdp14,autocracyFH



install.packages("plm")
library(plm)
?plm
est_3 <- plm(formula = lngdp14 ~ lndn13 + fiw + lndn13*fiw, effect = "twoways",
                         data = Estimations_data,model = "within", index = c("countryname","year"))
summary(est_3, cluster = "countryname")
est_3 <- plm(formula = lngdp14 ~ lndn13 + fiw + lndn13*fiw, effect = "twoways",
             data = Estimations_data,model = "within", index = c("countryname","year"))
coeftest(est_3, vcov = vcovHC)
est_4 <- plm(formula = lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_4)
coeftest(est_4, vcov = vcovHC)

est_1 <- plm(formula = lngdp14 ~ lndn13, effect = "twoways",
             data = Estimations_data,model = "within", index = c("countryname","year"))
summary(est_1)
est_2 <- plm(formula = lngdp14 ~ lndn13 + fiw, effect = "twoways",
             data = Estimations_data,model = "within", index = c("countryname","year"))
summary(est_2)
coeftest(est_2, vcov = vcovHC)
##################################################################
es_1184 <- Estimations_data %>% 
  select(countryname,year,lngdp14,lndn13,fiw)  %>% 
  mutate(sample=ifelse(is.na(fiw),0,1)) %>% 
  group_by(countryname) %>% 
  mutate(insubset=ifelse(sum(sample)>7,T,F)) %>% 
  filter(insubset) %>% 
  filter(year > 1991, year < 2014) 

# Table1_(1) --------------------------------------------------------------


ungroup() 


library(naniar)
n_complete(Estimations_data$fiw)
View(miss_var_summary(es_1184))

es_1184 <- Estimations_data %>% 
  select(countryname,year,lngdp14,lndn13,fiw)  %>% 
  drop_na(fiw)
est_1 <- plm(formula = lngdp14 ~ lndn13, effect = "twoways",
             data = es_1184,model = "within", index = c("countryname","year"))
summary(est_1)

?coeftest
library(lmtest)
coeftest(est_1, vcov = vcovHC)

est_5 <- plm(formula = lngdp14 ~ lndn13 + Dpfree+Dnfree + lndn13*Dpfree+lndn13*Dnfree, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_5)
coeftest(est_5, vcov = vcovHC)

est_6 <- plm(formula = lngdp14 ~ lndn13 + autocracyFH+lndn13*autocracyFH, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_6)
coeftest(est_6, vcov = vcovHC)
names(Estimations_data)

est_7 <- plm(formula = lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, effect = "twoways",
             data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_7)
coeftest(est_7, vcov = vcovHC)

est_7 <- plm(formula = lngdp14 ~ lndn13 + fiw + I(fiw^2) + lndn13*fiw, effect = "twoways",
               data = Estimations_data, model = "within", index = c("countryname","year"))
summary(est_7)
coeftest(est_7, vcov = vcovHC)
  
