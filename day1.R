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

fiw7 <- Estimations_data %>%
  select(year, countryname, fiw) %>% 
  pivot_wider(names_from = year, values_from = fiw) %>% 
  rename(fiw_1992="1992", fiw_1993="1993", fiw_2012="2012", fiw_2013="2013")

fiw7 <- fiw7 %>% 
  mutate(fiw9293=rowSums(fiw7[, c("fiw_1992", "fiw_1993")])/2,
         fiw1213=rowSums(fiw7[, c("fiw_2012", "fiw_2013")])/2)

ntl7 <- Estimations_data %>%
  select(year, countryname, lndn13) %>% 
  pivot_wider(names_from = year, values_from = lndn13) %>% 
  rename(lndn_1992="1992", lndn_1993="1993" , lndn_2012="2012", lndn_2013="2013")

ntl7 <- ntl7 %>% 
  mutate(ntl9293=rowSums(ntl7[, c("lndn_1992", "lndn_1993")])/2,
         ntl1213=rowSums(ntl7[, c("lndn_2012", "lndn_2013")])/2)

GDP7 <- Estimations_data %>%
  select(year, countryname, lngdp14) %>% 
  pivot_wider(names_from = year, values_from = lngdp14) %>% 
  rename(lngdp14_1992="1992", lngdp14_1993="1993" , lngdp14_2012="2012", lngdp14_2013="2013")

GDP7 <- GDP7 %>% 
  mutate(gdp9293=rowSums(GDP7[, c("lngdp14_1992", "lngdp14_1993")])/2,
         gdp1213=rowSums(GDP7[, c("lngdp14_2012", "lngdp14_2013")])/2)








